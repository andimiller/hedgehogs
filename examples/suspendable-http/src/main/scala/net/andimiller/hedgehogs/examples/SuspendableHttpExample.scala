package net.andimiller.hedgehogs.examples

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import cats.implicits._
import com.comcast.ip4s._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import net.andimiller.hedgehogs.DataGraph
import net.andimiller.hedgehogs.dag.visitor._
import net.andimiller.hedgehogs.dag.visitor.circe.DagSnapshotCodecs._
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder

import java.nio.file.{Files, Path, Paths}
import java.util.UUID
import scala.jdk.CollectionConverters._

/** A tiny end-to-end demo of the suspendable dag runner:
  *
  *   - POST /runs starts a run of a fixed dag; the "slow" node pretends to submit a job to some remote service and
  *     suspends, so the run is written to disk as JSON and the response tells you which job handles it's waiting on
  *   - you play the remote service: POST /jobs/<handle>/complete with {"result": <int>} is the webhook; the run owning
  *     that handle is rehydrated from disk, resumed, and saved again
  *   - GET /runs/<runId> shows the stored state at any point
  *
  * Runs are stored one JSON file per run under ./data/runs — a stand-in for a database table.
  */
object SuspendableHttpExample extends IOApp.Simple {

  sealed trait Node
  object Node {
    case class Constant(value: Int) extends Node
    case object Sum                 extends Node
    case object RemoteJob           extends Node

    implicit val encoder: Encoder[Node] = Encoder.instance {
      case Constant(value) => Json.obj("type" := "constant", "value" := value)
      case Sum             => Json.obj("type" := "sum")
      case RemoteJob       => Json.obj("type" := "remoteJob")
    }
    implicit val decoder: Decoder[Node] = Decoder.instance { c =>
      c.get[String]("type").flatMap {
        case "constant"  => c.get[Int]("value").map(Constant(_))
        case "sum"       => (Sum: Node).asRight
        case "remoteJob" => (RemoteJob: Node).asRight
        case other       => Left(DecodingFailure(s"Unknown Node type: $other", c.history))
      }
    }
  }

  type Snapshot = DagSnapshot[String, Node, Int, String, Unit]
  type Result   = RunResult[String, Node, Int, String, Unit]

  // total = (a + b) + remote-double-of-b; the remote job is played by you, over http
  val graph: DataGraph[String, Node, Unit] = DataGraph
    .empty[String, Node, Unit]
    .addNode("a", Node.Constant(3))
    .addNode("b", Node.Constant(4))
    .addNode("left", Node.Sum)
    .addNode("slow", Node.RemoteJob)
    .addNode("total", Node.Sum)
    .addEdge("a", "left", ())
    .addEdge("b", "left", ())
    .addEdge("b", "slow", ())
    .addEdge("left", "total", ())
    .addEdge("slow", "total", ())

  val visitor = new SimpleSuspendableDagVisitor[IO, String, Node, Int, Unit, String, Int] {
    override def run(id: String, node: Node, inputs: Map[String, Int]): IO[StepResult[Int, String]] =
      node match {
        case Node.Constant(value) => IO.pure(StepResult.Complete(value))
        case Node.Sum             => IO.pure(StepResult.Complete(inputs.values.sum))
        case Node.RemoteJob       =>
          for {
            handle <- IO(s"job-${UUID.randomUUID()}")
            _      <- IO.println(
                        s"[$id] submitted remote job $handle with inputs $inputs " +
                          s"(a real app would call the slow service here)"
                      )
          } yield StepResult.Suspend(handle)
      }

    override def resume(
        id: String,
        node: Node,
        handle: String,
        payload: Int,
        inputs: Map[String, Int]
    ): IO[StepResult[Int, String]] =
      node match {
        case Node.RemoteJob => IO.pure(StepResult.Complete(payload))
        case other          => IO.raiseError(new Throwable(s"$other never suspends"))
      }
  }

  // lifecycle logging via the runner's event callback
  val logEvents: RunEvent[String, String] => IO[Unit] = {
    case RunEvent.NodeStarted(id)           => IO.println(s"[events] running node $id")
    case RunEvent.NodeSuspended(id, handle) => IO.println(s"[events] node $id suspended, waiting on $handle")
    case RunEvent.NodeResumed(id, handle)   =>
      IO.println(s"[events] resuming node $id with callback $handle")
    case RunEvent.NodeCompleted(id)         => IO.println(s"[events] node $id completed")
  }

  // one JSON file per run under ./data/runs, standing in for a database table
  object store {
    val dir: Path = Paths.get("data", "runs")

    def runBody(runId: String, result: Result): Json = result match {
      case RunResult.Finished(g)         =>
        Json.obj(
          "runId"   := runId,
          "status"  := "finished",
          "outputs" := Json.fromFields(g.nodeMap.toList.map { case (id, out) => id -> out.asJson })
        )
      case RunResult.Suspended(snapshot) =>
        Json.obj(
          "runId"     := runId,
          "status"    := "suspended",
          "waitingOn" := snapshot.graph.nodeMap.toList.collect { case (id, NodeState.Waiting(_, handle)) =>
            Json.obj("node" := id, "handle" := (handle: String))
          },
          "snapshot"  := snapshot
        )
    }

    def save(runId: String, result: Result): IO[Json] = {
      val body = runBody(runId, result)
      IO.blocking(Files.writeString(dir.resolve(s"$runId.json"), body.spaces2)).as(body)
    }

    def load(runId: String): IO[Option[Json]] = {
      val path = dir.resolve(s"$runId.json")
      IO.blocking(Option.when(Files.exists(path))(Files.readString(path)))
        .flatMap(_.traverse(text => IO.fromEither(io.circe.parser.parse(text))))
    }

    /** Find the suspended run waiting on this handle. A real app would index handles in the database rather than
      * scanning; this is the `wait_handles` table from the design doc.
      */
    def findWaiting(handle: String): IO[Option[(String, Snapshot, String)]] =
      IO.blocking(Files.list(dir).iterator().asScala.toList)
        .flatMap(_.traverse(path => IO.blocking(Files.readString(path))))
        .map { files =>
          files
            .flatMap { text =>
              io.circe.parser.parse(text).toOption.flatMap { json =>
                (
                  json.hcursor.get[String]("runId").toOption,
                  json.hcursor.get[Snapshot]("snapshot").toOption
                ).tupled
              }
            }
            .collectFirstSome { case (runId, snapshot) =>
              snapshot.graph.nodeMap.collectFirst {
                case (nodeId, NodeState.Waiting(_, h)) if h == handle => (runId, snapshot, nodeId)
              }
            }
        }
  }

  case class JobResult(result: Int)
  object JobResult {
    implicit val decoder: Decoder[JobResult] = Decoder.instance(_.get[Int]("result").map(JobResult(_)))
  }

  // the lock plays the role of the database's single-writer-per-run guarantee (e.g. a
  // compare-and-swap on a revision column); here one global lock is plenty
  def routes(writeLock: Semaphore[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case POST -> Root / "runs" =>
      for {
        runId  <- IO(UUID.randomUUID().toString)
        result <- SuspendableDagVisitor.start(visitor, DagVisitor.RunMode.Flow, logEvents)(graph)
        body   <- store.save(runId, result)
        resp   <- Created(body)
      } yield resp

    case GET -> Root / "runs" / runId =>
      store.load(runId).flatMap {
        case Some(json) => Ok(json)
        case None       => NotFound(Json.obj("error" := s"no run $runId"))
      }

    case req @ POST -> Root / "jobs" / handle / "complete" =>
      writeLock.permit.use { _ =>
        for {
          payload <- req.as[JobResult]
          found   <- store.findWaiting(handle)
          resp    <- found match {
                       case None                            =>
                         NotFound(Json.obj("error" := s"no suspended run is waiting on $handle"))
                       case Some((runId, snapshot, nodeId)) =>
                         for {
                           _      <- IO.println(s"Rehydrating run $runId from database for callback $handle")
                           result <-
                             SuspendableDagVisitor.resume(visitor, logEvents)(snapshot, Map(nodeId -> payload.result))
                           body   <- store.save(runId, result)
                           r      <- Ok(body)
                         } yield r
                     }
        } yield resp
      }
  }

  val banner: String =
    """suspendable dag example listening on http://localhost:8080
      |
      |try:
      |  curl -s -X POST localhost:8080/runs                                    # start a run, note the handle
      |  curl -s -X POST localhost:8080/jobs/<handle>/complete -d '{"result": 8}'  # play the remote service
      |  curl -s localhost:8080/runs/<runId>                                    # inspect a run at any point
      |
      |runs are stored as JSON files under ./data/runs — restart the server between the
      |two POSTs to prove the resume really is rehydrating from disk
      |""".stripMargin

  override def run: IO[Unit] =
    for {
      _         <- IO.blocking(Files.createDirectories(store.dir))
      writeLock <- Semaphore[IO](1)
      _         <- EmberServerBuilder
                     .default[IO]
                     .withHost(host"0.0.0.0")
                     .withPort(port"8080")
                     .withHttpApp(routes(writeLock).orNotFound)
                     .build
                     .use(_ => IO.println(banner) *> IO.never)
    } yield ()
}
