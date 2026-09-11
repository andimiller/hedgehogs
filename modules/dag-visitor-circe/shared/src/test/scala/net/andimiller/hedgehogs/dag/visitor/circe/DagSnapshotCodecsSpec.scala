package net.andimiller.hedgehogs.dag.visitor.circe

import cats.effect.IO
import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import munit.CatsEffectSuite
import net.andimiller.hedgehogs.DataGraph
import net.andimiller.hedgehogs.dag.visitor._
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.RunMode
import net.andimiller.hedgehogs.dag.visitor.circe.DagSnapshotCodecs._

class DagSnapshotCodecsSpec extends CatsEffectSuite {

  sealed trait Node
  object Node {
    case class Local(value: Int)     extends Node
    case class Remote(jobId: String) extends Node
    case object Sum                  extends Node

    implicit val encoder: Encoder[Node] = Encoder.instance {
      case Local(value)  => Json.obj("type" := "local", "value" := value)
      case Remote(jobId) => Json.obj("type" := "remote", "jobId" := jobId)
      case Sum           => Json.obj("type" := "sum")
    }
    implicit val decoder: Decoder[Node] = Decoder.instance { c =>
      c.get[String]("type").flatMap {
        case "local"  => c.get[Int]("value").map(Local(_))
        case "remote" => c.get[String]("jobId").map(Remote(_))
        case "sum"    => (Sum: Node).asRight
        case other    => Left(DecodingFailure(s"Unknown Node type: $other", c.history))
      }
    }
  }

  val snapshot: DagSnapshot[String, Node, Int, String, Unit] = DagSnapshot(
    DataGraph
      .empty[String, NodeState[Node, Int, String], Unit]
      .addNode("A", NodeState.Done(1))
      .addNode("B", NodeState.Waiting(Node.Remote("job-b"), "job-b"))
      .addNode("C", NodeState.Pending(Node.Sum))
      .addEdge("A", "C", ())
      .addEdge("B", "C", ()),
    RunMode.Flow
  )

  test("Round-trip a snapshot through JSON") {
    assertEquals(
      snapshot.asJson.as[DagSnapshot[String, Node, Int, String, Unit]],
      Right(snapshot)
    )
  }

  test("Round-trip a snapshot through printed JSON text") {
    assertEquals(
      io.circe.parser.decode[DagSnapshot[String, Node, Int, String, Unit]](snapshot.asJson.noSpaces),
      Right(snapshot)
    )
  }

  test("Round-trip both run modes") {
    List[RunMode](RunMode.Flow, RunMode.Dependency).foreach { mode =>
      assertEquals(mode.asJson.as[RunMode], Right(mode))
    }
  }

  test("Reject unknown node state types") {
    assert(
      Json
        .obj("type" := "exploded")
        .as[NodeState[Node, Int, String]]
        .isLeft
    )
  }

  test("Reject unknown run modes") {
    assert(Json.fromString("sideways").as[RunMode].isLeft)
  }

  test("Suspend, persist to JSON, rehydrate, and resume through to Finished") {

    // this is the full application flow: run until suspended, store the snapshot
    // somewhere as JSON, and later (new process, new graph objects) decode it and
    // resume with the remote job's result
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

    val graph = DataGraph
      .empty[String, Node, Unit]
      .addNode("A", Node.Local(1))
      .addNode("B", Node.Remote("job-b"))
      .addNode("C", Node.Sum)
      .addEdge("A", "C", ())
      .addEdge("B", "C", ())

    val expected = DataGraph
      .empty[String, Int, Unit]
      .addNode("A", 1)
      .addNode("B", 10)
      .addNode("C", 11)
      .addEdge("A", "C", ())
      .addEdge("B", "C", ())

    for {
      first     <- SuspendableDagVisitor.start(visitor)(graph)
      persisted <- first match {
                     case RunResult.Suspended(s) => IO.pure(s.asJson.noSpaces)
                     case other                  => IO.raiseError(new Throwable(s"expected Suspended, got $other"))
                   }
      restored  <- IO.fromEither(
                     io.circe.parser.decode[DagSnapshot[String, Node, Int, String, Unit]](persisted)
                   )
      second    <- SuspendableDagVisitor.resume(visitor)(restored, Map("B" -> 10))
      _          = assertEquals(second, RunResult.Finished[String, Node, Int, String, Unit](expected))
    } yield ()
  }
}
