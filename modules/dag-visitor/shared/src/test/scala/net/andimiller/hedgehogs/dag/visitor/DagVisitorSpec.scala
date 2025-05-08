package net.andimiller.hedgehogs.dag.visitor

import cats.effect.kernel.Outcome.Errored
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import munit.CatsEffectSuite
import net.andimiller.hedgehogs.DataGraph
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.RunMode.Dependency

import scala.concurrent.duration.DurationInt

class DagVisitorSpec extends CatsEffectSuite {

  test("Run a simple maths dag") {

    sealed trait Node
    object Node {
      case class Literal(i: Int) extends Node
      case object Multiply       extends Node
      case object Add            extends Node
    }
    case object Takes

    val graph = DataGraph
      .empty[String, Node, Takes.type]
      .addNode("A", Node.Literal(1))
      .addNode("B", Node.Literal(2))
      .addNode("C", Node.Literal(3))
      .addNode("D", Node.Add)
      .addNode("E", Node.Multiply)
      .addEdge("A", "D", Takes)
      .addEdge("B", "D", Takes)
      .addEdge("C", "E", Takes)
      .addEdge("D", "E", Takes)

    val runner = new SimpleDagVisitor[IO, String, Node, Int, Takes.type] {
      override def run(id: String, node: Node, inputs: Map[String, Int]): IO[Int] = {
        node match {
          case Node.Literal(i) => IO.pure(i)
          case Node.Multiply   =>
            IO {
              inputs.values.product
            }
          case Node.Add        =>
            IO {
              inputs.values.sum
            }
        }
      }
    }

    val expected =
      DataGraph
        .empty[String, Int, Takes.type]
        .addNode("A", 1)
        .addNode("B", 2)
        .addNode("C", 3)
        .addNode("D", 3)
        .addNode("E", 9)
        .addEdge("A", "D", Takes)
        .addEdge("B", "D", Takes)
        .addEdge("C", "E", Takes)
        .addEdge("D", "E", Takes)

    DagVisitor
      .runConcurrent(runner)(graph)
      .assertEquals(expected) *> // and run it the other way
      DagVisitor
        .runConcurrent(runner, direction = Dependency)(graph.reverse)
        .assertEquals(expected.reverse)

  }

  test("Cancel subtasks correctly") {

    case class SleepForSeconds(seconds: Int)

    for {
      started   <- Ref.of[IO, Set[String]](Set.empty)
      ended     <- Ref.of[IO, Set[String]](Set.empty)
      cancelled <- Ref.of[IO, Set[String]](Set.empty)
      graph      = DataGraph
                     .empty[String, SleepForSeconds, Unit]
                     .addNode("A", SleepForSeconds(5))
                     .addNode("B", SleepForSeconds(11))
                     .addNode("C", SleepForSeconds(11))
                     .addNode("D", SleepForSeconds(4))
                     .addNode("E", SleepForSeconds(13))
                     .addNode("F", SleepForSeconds(7))
                     .addEdge("A", "B", ())
                     .addEdge("C", "D", ())
      runner     = new SimpleDagVisitor[IO, String, SleepForSeconds, Unit, Unit] {
                     override def run(id: String, node: SleepForSeconds, inputs: Map[String, Unit]): IO[Unit] =
                       (started.update(_ + id) *> IO.sleep(node.seconds.seconds) *> ended.update(_ + id))
                         .onCancel(cancelled.update(_ + id))
                   }
      _         <- TestControl
                     .execute(
                       DagVisitor.runConcurrent(runner)(graph).timeout(10.seconds).void
                     )
                     .flatMap { control =>
                       control.tickFor(10.seconds) *> control.results
                     }
                     .map { case Some(Errored(e)) =>
                       e.toString
                     }
                     .assertEquals("java.util.concurrent.TimeoutException: 10 seconds")
      _         <- started.get.assertEquals(Set("A", "B", "C", "E", "F"))
      _         <- ended.get.assertEquals(Set("A", "F"))
      _         <- cancelled.get.assertEquals(Set("B", "C", "E"))
    } yield ()

  }

  test("Stop early if one subtask fails") {

    sealed trait Node
    case class SleepForSeconds(seconds: Int) extends Node
    case class Boom(msg: String)             extends Node

    for {
      started   <- Ref.of[IO, Set[String]](Set.empty)
      ended     <- Ref.of[IO, Set[String]](Set.empty)
      errored   <- Ref.of[IO, Set[String]](Set.empty)
      cancelled <- Ref.of[IO, Set[String]](Set.empty)
      graph      = DataGraph
                     .empty[String, Node, Unit]
                     .addNode("A", SleepForSeconds(5))
                     .addNode("B", SleepForSeconds(11))
                     .addNode("C", SleepForSeconds(11))
                     .addNode("D", SleepForSeconds(4))
                     .addNode("E", SleepForSeconds(13))
                     .addNode("F", SleepForSeconds(7))
                     .addNode("G", Boom("G went boom"))
                     .addEdge("A", "B", ())
                     .addEdge("C", "D", ())
                     .addEdge("A", "G", ())
      runner     = new SimpleDagVisitor[IO, String, Node, Unit, Unit] {
                     override def run(id: String, node: Node, inputs: Map[String, Unit]): IO[Unit] = {
                       ((started.update(_ + id)) *> (node match {
                         case SleepForSeconds(seconds) => IO.sleep(seconds.seconds)
                         case Boom(msg)                => IO.raiseError(new Throwable(msg))
                       }) *> ended.update(_ + id))
                         .onError(_ => errored.update(_ + id))
                         .onCancel(cancelled.update(_ + id))
                     }
                   }
      _         <- TestControl
                     .execute(
                       DagVisitor.runConcurrent(runner)(graph).timeout(10.seconds).void
                     )
                     .flatMap { control =>
                       control.tickFor(10.seconds) *> control.results
                     }
                     .map { case Some(Errored(e)) =>
                       e.toString
                     }
                     .assertEquals(
                       "net.andimiller.hedgehogs.dag.visitor.DagVisitor$SubtaskFailed: Node G failed to run: G went boom"
                     )
      _         <- errored.get.assertEquals(Set("G"))
      _         <- started.get.assertEquals(Set("A", "B", "C", "C", "E", "F", "G"))
      _         <- ended.get.assertEquals(Set("A"))
      _         <- cancelled.get.assertEquals(Set("B", "C", "E", "F"))
    } yield ()

  }
}
