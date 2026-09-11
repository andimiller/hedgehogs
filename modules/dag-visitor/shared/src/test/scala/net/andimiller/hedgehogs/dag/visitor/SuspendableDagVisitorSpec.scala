package net.andimiller.hedgehogs.dag.visitor

import cats.effect.kernel.Outcome.Errored
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import cats.syntax.all._
import munit.CatsEffectSuite
import net.andimiller.hedgehogs.DataGraph
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.RunMode

import scala.concurrent.duration.DurationInt

class SuspendableDagVisitorSpec extends CatsEffectSuite {

  sealed trait Node
  object Node {
    case class Local(value: Int)     extends Node
    case class Remote(jobId: String) extends Node
    case object Sum                  extends Node
  }

  // completes local nodes immediately, suspends remote ones, and completes a resumed
  // remote node with whatever payload it was given
  val visitor = new SimpleSuspendableDagVisitor[IO, String, Node, Int, Unit, String, Int] {
    override def run(id: String, node: Node, inputs: Map[String, Int]): IO[StepResult[Int, String]] =
      node match {
        case Node.Local(v)      => IO.pure(StepResult.Complete(v))
        case Node.Sum           => IO(StepResult.Complete(inputs.values.sum))
        case Node.Remote(jobId) => IO.pure(StepResult.Suspend(jobId))
      }

    override def resume(
        id: String,
        node: Node,
        handle: String,
        payload: Int,
        inputs: Map[String, Int]
    ): IO[StepResult[Int, String]] =
      IO.pure(StepResult.Complete(payload))
  }

  test("Run a dag with no suspensions to Finished, equivalently to runConcurrent") {

    val graph = DataGraph
      .empty[String, Node, Unit]
      .addNode("A", Node.Local(1))
      .addNode("B", Node.Local(2))
      .addNode("C", Node.Local(3))
      .addNode("D", Node.Sum)
      .addNode("E", Node.Sum)
      .addEdge("A", "D", ())
      .addEdge("B", "D", ())
      .addEdge("C", "E", ())
      .addEdge("D", "E", ())

    val expected = DataGraph
      .empty[String, Int, Unit]
      .addNode("A", 1)
      .addNode("B", 2)
      .addNode("C", 3)
      .addNode("D", 3)
      .addNode("E", 6)
      .addEdge("A", "D", ())
      .addEdge("B", "D", ())
      .addEdge("C", "E", ())
      .addEdge("D", "E", ())

    val equivalentRunner = new SimpleDagVisitor[IO, String, Node, Int, Unit] {
      override def run(id: String, node: Node, inputs: Map[String, Int]): IO[Int] =
        node match {
          case Node.Local(v)  => IO.pure(v)
          case Node.Sum       => IO(inputs.values.sum)
          case Node.Remote(_) => IO.raiseError(new Throwable("no remotes in this test"))
        }
    }

    SuspendableDagVisitor
      .start(visitor)(graph)
      .assertEquals(RunResult.Finished[String, Node, Int, String, Unit](expected)) *>
      DagVisitor.runConcurrent(equivalentRunner)(graph).assertEquals(expected) *>
      // and the other direction
      SuspendableDagVisitor
        .start(visitor, direction = RunMode.Dependency)(graph.reverse)
        .assertEquals(RunResult.Finished[String, Node, Int, String, Unit](expected.reverse))
  }

  test("Run an empty dag to Finished") {
    SuspendableDagVisitor
      .start(visitor)(DataGraph.empty[String, Node, Unit])
      .assertEquals(RunResult.Finished[String, Node, Int, String, Unit](DataGraph.empty[String, Int, Unit]))
  }

  val suspendingGraph = DataGraph
    .empty[String, Node, Unit]
    .addNode("A", Node.Local(1))
    .addNode("B", Node.Remote("job-b"))
    .addNode("C", Node.Sum)
    .addNode("D", Node.Local(4))
    .addEdge("A", "C", ())
    .addEdge("B", "C", ())

  val suspendedSnapshot = DagSnapshot(
    DataGraph
      .empty[String, NodeState[Node, Int, String], Unit]
      .addNode("A", NodeState.Done(1))
      .addNode("B", NodeState.Waiting(Node.Remote("job-b"), "job-b"))
      .addNode("C", NodeState.Pending(Node.Sum))
      .addNode("D", NodeState.Done(4))
      .addEdge("A", "C", ())
      .addEdge("B", "C", ()),
    RunMode.Flow
  )

  test("Suspend mid-graph, with independent branches still completing") {
    SuspendableDagVisitor
      .start(visitor)(suspendingGraph)
      .assertEquals(RunResult.Suspended(suspendedSnapshot))
  }

  test("Resume a suspended dag through to Finished") {

    val expected = DataGraph
      .empty[String, Int, Unit]
      .addNode("A", 1)
      .addNode("B", 10)
      .addNode("C", 11)
      .addNode("D", 4)
      .addEdge("A", "C", ())
      .addEdge("B", "C", ())

    SuspendableDagVisitor
      .resume(visitor)(suspendedSnapshot, Map("B" -> 10))
      .assertEquals(RunResult.Finished[String, Node, Int, String, Unit](expected))
  }

  test("Resume can suspend again") {

    // treats a negative payload as "job not done yet, poll again with a new handle"
    val retrying = new SimpleSuspendableDagVisitor[IO, String, Node, Int, Unit, String, Int] {
      override def run(id: String, node: Node, inputs: Map[String, Int]): IO[StepResult[Int, String]] =
        visitor.run(id, node, inputs)

      override def resume(
          id: String,
          node: Node,
          handle: String,
          payload: Int,
          inputs: Map[String, Int]
      ): IO[StepResult[Int, String]] =
        if (payload < 0) IO.pure(StepResult.Suspend(s"$handle-retry"))
        else IO.pure(StepResult.Complete(payload))
    }

    val resuspended = DagSnapshot(
      suspendedSnapshot.graph.addNode("B", NodeState.Waiting(Node.Remote("job-b"), "job-b-retry")),
      RunMode.Flow
    )

    val expected = DataGraph
      .empty[String, Int, Unit]
      .addNode("A", 1)
      .addNode("B", 10)
      .addNode("C", 11)
      .addNode("D", 4)
      .addEdge("A", "C", ())
      .addEdge("B", "C", ())

    for {
      first  <- SuspendableDagVisitor.resume(retrying)(suspendedSnapshot, Map("B" -> -1))
      _       = assertEquals(first, RunResult.Suspended(resuspended))
      second <- SuspendableDagVisitor.resume(retrying)(resuspended, Map("B" -> 10))
      _       = assertEquals(second, RunResult.Finished[String, Node, Int, String, Unit](expected))
    } yield ()
  }

  test("Stay suspended when only some waiting nodes are completed") {

    val graph = DataGraph
      .empty[String, Node, Unit]
      .addNode("B", Node.Remote("job-b"))
      .addNode("E", Node.Remote("job-e"))

    val bothWaiting = DagSnapshot(
      DataGraph
        .empty[String, NodeState[Node, Int, String], Unit]
        .addNode("B", NodeState.Waiting(Node.Remote("job-b"), "job-b"))
        .addNode("E", NodeState.Waiting(Node.Remote("job-e"), "job-e")),
      RunMode.Flow
    )

    val oneCompleted = DagSnapshot(
      bothWaiting.graph.addNode("B", NodeState.Done(10)),
      RunMode.Flow
    )

    for {
      first  <- SuspendableDagVisitor.start(visitor)(graph)
      _       = assertEquals(first, RunResult.Suspended(bothWaiting))
      second <- SuspendableDagVisitor.resume(visitor)(bothWaiting, Map("B" -> 10))
      _       = assertEquals(second, RunResult.Suspended(oneCompleted))
    } yield ()
  }

  test("Emit run events for started, suspended, resumed, and completed nodes") {
    for {
      seen  <- Ref.of[IO, Vector[RunEvent[String, String]]](Vector.empty)
      log    = (e: RunEvent[String, String]) => seen.update(_ :+ e)
      _     <- SuspendableDagVisitor.start(visitor, RunMode.Flow, log)(suspendingGraph)
      _     <- seen.get
                 .map(_.toSet)
                 .assertEquals(
                   Set[RunEvent[String, String]](
                     RunEvent.NodeStarted("A"),
                     RunEvent.NodeStarted("B"),
                     RunEvent.NodeStarted("D"),
                     RunEvent.NodeCompleted("A"),
                     RunEvent.NodeCompleted("D"),
                     RunEvent.NodeSuspended("B", "job-b")
                   )
                 )
      _     <- seen.set(Vector.empty)
      _     <- SuspendableDagVisitor.resume(visitor, log)(suspendedSnapshot, Map("B" -> 10))
      _     <- seen.get
                 .map(_.toSet)
                 .assertEquals(
                   Set[RunEvent[String, String]](
                     RunEvent.NodeResumed("B", "job-b"),
                     RunEvent.NodeCompleted("B"),
                     RunEvent.NodeStarted("C"),
                     RunEvent.NodeCompleted("C")
                   )
                 )
      // per-node ordering is guaranteed: resumption is announced before completion
      _     <- seen.set(Vector.empty)
      _     <- SuspendableDagVisitor.resume(visitor, log)(suspendedSnapshot, Map("B" -> 10))
      order <- seen.get.map(_.collect {
                 case RunEvent.NodeResumed("B", _) => "resumed"
                 case RunEvent.NodeCompleted("B")  => "completed"
               })
      _      = assertEquals(order, Vector("resumed", "completed"))
    } yield ()
  }

  test("Reject completions for unknown or non-waiting nodes") {
    interceptIO[SuspendableDagVisitor.InvalidCompletions] {
      SuspendableDagVisitor.resume(visitor)(suspendedSnapshot, Map("A" -> 5))
    } *>
      interceptIO[SuspendableDagVisitor.InvalidCompletions] {
        SuspendableDagVisitor.resume(visitor)(suspendedSnapshot, Map("Z" -> 5))
      }
  }

  test("Reject cyclic input immediately") {
    val cyclic = DataGraph
      .empty[String, Node, Unit]
      .addNode("A", Node.Local(1))
      .addNode("B", Node.Local(2))
      .addEdge("A", "B", ())
      .addEdge("B", "A", ())

    interceptIO[SuspendableDagVisitor.InvalidDag] {
      SuspendableDagVisitor.start(visitor)(cyclic)
    }
  }

  test("Fail fast and cancel in-flight fibers when a resumed node errors") {

    sealed trait N
    case class Slow(seconds: Int) extends N
    case object RemoteBoom        extends N

    val snapshot = DagSnapshot(
      DataGraph
        .empty[String, NodeState[N, Unit, String], Unit]
        .addNode("B", NodeState.Waiting(RemoteBoom: N, "job-b"))
        .addNode("F", NodeState.Pending(Slow(10): N)),
      RunMode.Flow
    )

    for {
      cancelled <- Ref.of[IO, Set[String]](Set.empty)
      runner     = new SimpleSuspendableDagVisitor[IO, String, N, Unit, Unit, String, Unit] {
                     override def run(id: String, node: N, inputs: Map[String, Unit]): IO[StepResult[Unit, String]] =
                       node match {
                         case Slow(seconds) =>
                           IO.sleep(seconds.seconds)
                             .as(StepResult.Complete(()): StepResult[Unit, String])
                             .onCancel(cancelled.update(_ + id))
                         case RemoteBoom    => IO.pure(StepResult.Suspend("job-b"))
                       }

                     override def resume(
                         id: String,
                         node: N,
                         handle: String,
                         payload: Unit,
                         inputs: Map[String, Unit]
                     ): IO[StepResult[Unit, String]] =
                       IO.sleep(1.second) *> IO.raiseError(new Throwable("boom"))
                   }
      _         <- TestControl
                     .execute(
                       SuspendableDagVisitor.resume(runner)(snapshot, Map("B" -> ())).void
                     )
                     .flatMap { control =>
                       control.tickAll *> control.results
                     }
                     .map {
                       case Some(Errored(e)) => e.toString
                       case other            => fail(s"Expected Some(Errored(_)) but got $other")
                     }
                     .assertEquals(
                       "net.andimiller.hedgehogs.dag.visitor.DagVisitor$SubtaskFailed: Node B failed to run: boom"
                     )
      _         <- cancelled.get.assertEquals(Set("F"))
    } yield ()
  }
}
