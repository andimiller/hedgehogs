package net.andimiller.hedgehogs.dag.visitor

import net.andimiller.hedgehogs.DataGraph
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.RunMode
import net.andimiller.hedgehogs.dag.visitor.circe.DagSnapshotCodecs.given
import cats.effect.IO
import cats.implicits.*
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*
import tyrian.Html.*
import tyrian.SVG.*
import tyrian.*
import cats.effect.std.Queue
import cats.Show
import org.scalajs.dom.window

import scala.scalajs.js.annotation.*
import scala.concurrent.duration.*
import java.time.{Instant, Duration}
import scala.util.Random
import net.andimiller.hedgehogs.Dag

type Snap = DagSnapshot[String, Node, Node, String, Unit]

enum Node:
  case WaitSeconds(n: FiniteDuration)
  case Approval
object Node:
  given Show[Node] = new Show[Node] {
    override def show(n: Node): String =
      n match
        case WaitSeconds(n) =>
          n.toString.replace(" ", "").replace("econds", "").replace("econd", "") // quickly make it shorter
        case Approval => "Approve?"
  }

  // DagSnapshot embeds node inputs/outputs, so Node needs codecs; FiniteDuration has none so we write them by hand
  given Encoder[Node] = Encoder.instance {
    case WaitSeconds(n) => Json.obj("type" := "waitSeconds", "seconds" := n.toSeconds)
    case Approval       => Json.obj("type" := "approval")
  }
  given Decoder[Node] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "waitSeconds" => c.get[Long]("seconds").map(s => WaitSeconds(s.seconds))
      case "approval"    => Right(Approval)
      case other         => Left(DecodingFailure(s"Unknown Node type: $other", c.history))
    }
  }

enum State:
  case Pending, Running, Suspended, Done
  def toColour: String  =
    this match
      case Pending   => "indianred"
      case Running   => "coral"
      case Suspended => "dodgerblue"
      case Done      => "limegreen"
  def toHtml: Html[Msg] = Html.span(style := s"color:${this.toColour};")(this.toString)

case class Model(
    startTime: Instant,
    graph: DataGraph[String, (Node, State), Unit],
    messages: Vector[Html[Msg]],
    queue: Option[Queue[IO, Msg]],
    nodes: Int = 5,
    edges: Int = 5,
    approvals: Int = 0,
    running: Boolean = false,
    snapshot: Option[Snap] = None,
    snapshotText: String = "",
    graphviz: Option[Graphviz] = None
)

@JSExportTopLevel("TyrianApp")
object Demo extends TyrianIOApp[Msg, Model]:

  val nodeNames: Seq[String] = (('A' to 'Z') ++ ('a' to 'z')).map(_.toString)

  val storageKey: String = "hedgehogs-dag-snapshot"

  def makeProgram(nodes: Int, edges: Int, approvals: Int): DataGraph[String, Node, Unit] = {
    val nodeGraph   = nodeNames
      .take(nodes)
      .foldLeft(
        DataGraph.empty[String, Node, Unit]
      ) { case (g, n) =>
        g.addNode(n.toString, Node.WaitSeconds(1.seconds))
      }
    val nodesVector = nodeGraph.nodes.toVector
    val withEdges   = (0 to edges).foldLeft(nodeGraph) { case (g, _) =>
      val from = nodesVector(Random.between(0, g.nodes.size.toInt))
      val to   = nodesVector(Random.between(0, g.nodes.size.toInt))
      if (from != to) {
        val nextGraph = g.addEdge(from, to, ())
        if (Dag.isDag(nextGraph).value) {
          nextGraph
        } else {
          g
        }
      } else {
        g
      }
    }
    Random.shuffle(nodesVector).take(approvals.min(nodes)).foldLeft(withEdges) { case (g, id) =>
      g.addNode(id, Node.Approval)
    }
  }

  // the visitor returns its input as its output, so a snapshot's Done nodes still know what they were
  val visitor: SimpleSuspendableDagVisitor[IO, String, Node, Node, Unit, String, String] =
    new SimpleSuspendableDagVisitor[IO, String, Node, Node, Unit, String, String] {
      override def run(id: String, node: Node, inputs: Map[String, Node]): IO[StepResult[Node, String]] =
        node match
          case Node.WaitSeconds(n) => IO.sleep(n).as(StepResult.Complete(node))
          case Node.Approval       => IO.sleep(500.millis).as(StepResult.Suspend(s"approval-$id"))

      override def resume(
          id: String,
          node: Node,
          handle: String,
          payload: String,
          inputs: Map[String, Node]
      ): IO[StepResult[Node, String]]                                                                   =
        IO.sleep(500.millis).as(StepResult.Complete(node))
    }

  def events(queue: Queue[IO, Msg]): RunEvent[String, String] => IO[Unit] = {
    case RunEvent.NodeStarted(id)      => queue.offer(Msg.UpdateNodeState(id, State.Running))
    case RunEvent.NodeSuspended(id, _) => queue.offer(Msg.UpdateNodeState(id, State.Suspended))
    case RunEvent.NodeResumed(id, _)   => queue.offer(Msg.UpdateNodeState(id, State.Running))
    case RunEvent.NodeCompleted(id)    => queue.offer(Msg.UpdateNodeState(id, State.Done))
  }

  // the run result goes through the same queue as the node events, so it can't overtake them in the log
  def foldRun(queue: Queue[IO, Msg])(run: IO[RunResult[String, Node, Node, String, Unit]]): IO[Msg] =
    run.attempt
      .map(_.fold(e => Msg.RunFailed(e.getMessage), Msg.RunCompleted.apply))
      .flatMap(queue.offer)
      .as(Msg.NoOp)

  def waitingIds(snap: Snap): List[String] =
    snap.graph.nodeMap.collect { case (id, NodeState.Waiting(_, _)) => id }.toList.sorted

  def router: Location => Msg              = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (
      Model(Instant.now(), makeProgram(5, 5, 0).mapNode(n => (n, State.Pending)), Vector(), None),
      Cmd.Batch(
        Cmd.Run(Queue.unbounded[IO, Msg].map(Msg.QueueCreated.apply)),
        Cmd.Emit(Msg.Reroll),
        Cmd.Emit(Msg.LoadGraphviz)
      )
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.NoOp                           => (model, Cmd.None)
    case Msg.Start if model.running         => (model, Cmd.None)
    case Msg.Start                          =>
      (
        model.copy(
          startTime = Instant.now(),
          graph = model.graph.mapNode { case (n, _) => n -> State.Pending },
          running = true,
          snapshot = None,
          snapshotText = ""
        ),
        Cmd.Batch(
          Cmd.Emit(Msg.ClearLog),
          Cmd.Emit(Msg.SyncNodeColours),
          Cmd.Run(
            model.queue match
              case Some(q) =>
                foldRun(q)(SuspendableDagVisitor.start(visitor, RunMode.Flow, events(q))(model.graph.mapNode(_._1)))
              case None    => IO.pure(Msg.RunFailed("event queue not ready yet"))
          )
        )
      )
    case Msg.Approve(id)                    =>
      model.snapshot match
        case Some(snap) if !model.running =>
          (
            // clear the snapshot immediately: never resume or save a stale one, and hide the other Approve buttons
            model.copy(running = true, snapshot = None, snapshotText = ""),
            Cmd.Batch(
              Cmd.Emit(Msg.Log(Html.span(text(s"$id was "), Html.span(style := "color:limegreen;")("approved")))),
              Cmd.Run(
                model.queue match
                  case Some(q) =>
                    foldRun(q)(SuspendableDagVisitor.resume(visitor, events(q))(snap, Map(id -> "approved")))
                  case None    => IO.pure(Msg.RunFailed("event queue not ready yet"))
              )
            )
          )
        case _                            => (model, Cmd.None)
    case Msg.RunCompleted(result)           =>
      result match
        case RunResult.Finished(_)     =>
          (
            model.copy(running = false, snapshot = None, snapshotText = ""),
            Cmd.Emit(Msg.Log(Html.span(text("Run finished, every node is "), State.Done.toHtml)))
          )
        case RunResult.Suspended(snap) =>
          (
            model.copy(running = false, snapshot = Some(snap), snapshotText = snap.asJson.spaces2),
            Cmd.Emit(
              Msg.Log(
                Html.span(text(s"Run suspended, waiting for approval of: ${waitingIds(snap).mkString(", ")}"))
              )
            )
          )
    case Msg.RunFailed(message)             =>
      (
        model.copy(running = false),
        Cmd.Emit(Msg.Log(Html.span(style := "color:indianred;")(s"Run failed: $message")))
      )
    case Msg.ApplySnapshot if model.running => (model, Cmd.None)
    case Msg.ApplySnapshot                  =>
      io.circe.parser.decode[Snap](model.snapshotText) match
        case Left(err)   =>
          (
            model,
            Cmd.Emit(Msg.Log(Html.span(style := "color:indianred;")(s"Could not parse snapshot: ${err.getMessage}")))
          )
        case Right(snap) =>
          val newGraph = snap.graph.mapNode {
            case NodeState.Pending(n)    => n -> State.Pending
            case NodeState.Waiting(n, _) => n -> State.Suspended
            case NodeState.Done(n)       => n -> State.Done
          }
          (
            model.copy(graph = newGraph, snapshot = Some(snap)),
            Cmd.Batch(
              Cmd.Emit(
                Msg.Log(
                  Html.span(text(s"Snapshot applied, waiting for approval of: ${waitingIds(snap).mkString(", ")}"))
                )
              ),
              Cmd.Emit(Msg.SyncNodeColours)
            )
          )
    case Msg.SaveSnapshot                   =>
      if (model.snapshotText.isEmpty)
        (model, Cmd.Emit(Msg.Log(Html.span(text("No snapshot to save")))))
      else
        (
          model,
          Cmd.Batch(
            Cmd.SideEffect { window.localStorage.setItem(storageKey, model.snapshotText) },
            Cmd.Emit(Msg.Log(Html.span(text("Snapshot saved to localStorage"))))
          )
        )
    case Msg.LoadSnapshot if model.running  => (model, Cmd.None)
    case Msg.LoadSnapshot                   =>
      (
        model,
        Cmd.Run(
          IO(Option(window.localStorage.getItem(storageKey))).map {
            case Some(saved) => Msg.SnapshotLoaded(saved)
            case None        => Msg.Log(Html.span(text("No snapshot found in localStorage")))
          }
        )
      )
    case Msg.SnapshotLoaded(saved)          =>
      (model.copy(snapshotText = saved), Cmd.Emit(Msg.ApplySnapshot))
    case Msg.Reroll if model.running        => (model, Cmd.None)
    case Msg.Reroll                         =>
      val newGraph = makeProgram(model.nodes, model.edges, model.approvals).mapNode(n => (n, State.Pending))
      (
        model.copy(
          startTime = Instant.now(),
          graph = newGraph,
          snapshot = None,
          snapshotText = ""
        ),
        Cmd.Batch(
          Cmd.Emit(Msg.ClearLog),
          Cmd.Emit(
            Msg.Log(
              Html.span(
                text(
                  s"Graph has been regenerated with ${newGraph.nodes.size} nodes and ${newGraph.edges.size} edges, all nodes reset to "
                ),
                State.Pending.toHtml
              )
            )
          ),
          Cmd.Emit(Msg.SyncNodeColours)
        )
      )
    case Msg.LoadGraphviz                   =>
      (
        model,
        Cmd.Run(GraphvizIO.load().map(Msg.GraphvizLoaded.apply))
      )
    case Msg.GraphvizLoaded(g)              =>
      (
        model.copy(graphviz = Some(g)),
        Cmd.Emit(Msg.Log(Html.span(text("Graphviz loaded"))))
      )
    case Msg.NodeCount(n)                   =>
      (
        model.copy(
          nodes = n
        ),
        Cmd.Emit(Msg.Reroll)
      )
    case Msg.EdgeCount(e)                   =>
      (
        model.copy(
          edges = e
        ),
        Cmd.Emit(Msg.Reroll)
      )
    case Msg.ApprovalCount(a)               =>
      (
        model.copy(
          approvals = a
        ),
        Cmd.Emit(Msg.Reroll)
      )
    case Msg.QueueCreated(q)                =>
      (model.copy(queue = Some(q)), Cmd.None)
    case Msg.SyncNodeColours                =>
      (
        model,
        Cmd.Batch(
          model.graph.mapNode(_._2).nodeMap.toList.map { case (id, state) =>
            CssVariables.set[IO, Msg](s"--$id-fill", state.toColour)
          }
        )
      )
    case Msg.UpdateNodeState(id, s)         =>
      val nextGraph   = model.graph.addNode(id, model.graph.nodeMap(id)._1 -> s)
      val stateUpdate = CssVariables.set[IO, Msg](s"--$id-fill", s.toColour)
      (
        model.copy(
          graph = nextGraph
        ),
        Cmd.Batch(
          List(
            Msg.Log(Html.span(text(s"$id changed state to "), s.toHtml)).some,
            Option.when(nextGraph.nodeMap.values.map(_._2).forall(_ == State.Done))(
              Msg.Log(Html.span(text("Graph is all "), State.Done.toHtml))
            )
          ).flatten.map(Cmd.Emit.apply).appended(stateUpdate)
        )
      )
    case Msg.Log(msg)                       =>
      val now = Instant.now()
      val ts  = Duration.between(model.startTime, now).getSeconds()
      (
        model.copy(
          messages = model.messages.appended(Html.span(text(s"${ts}s "), msg))
        ),
        Cmd.None
      )
    case Msg.ClearLog                       =>
      (model.copy(messages = Vector()), Cmd.None)

  def slider(
      name: String,
      htmlId: String,
      min: Int,
      max: Int,
      initial: Int,
      current: Int,
      onChanged: Int => Msg
  ): Html[Msg] =
    label(
      attr("for")    := htmlId,
      style          := "display: flex; align-items: center; gap: 8px; width: 100%;"
    )(
      Html.span(style := "flex: 0 0 80px;")(name),
      input(
        id           := htmlId,
        attr("type") := "range",
        attr("min")  := min.toString,
        value        := initial.toString,
        attr("max")  := max.toString,
        style        := "flex: 1 1 auto; margin: 0;",
        onChange(value => onChanged(value.toInt))
      ),
      Html.span(style := "flex: 0 0 32px; text-align: right;")(current.toString)
    )

  def view(model: Model): Html[Msg] =
    div(
      style := "display: flex; gap: 24px; align-items: flex-start;"
    )(
      div(
        style := "flex: 0 0 400px;"
      )(
        (List[Html[Msg]](
          div(
            h2("Controls"),
            slider("Nodes", "nodes", 2, 50, 5, model.nodes, Msg.NodeCount.apply),
            hr,
            slider("Edges", "edges", 2, 100, 10, model.edges, Msg.EdgeCount.apply),
            hr,
            slider("Approvals", "approvals", 0, model.nodes, 0, model.approvals, Msg.ApprovalCount.apply),
            hr,
            button(onClick(Msg.Reroll))("Reroll"),
            button(onClick(Msg.Start))("Start")
          )
        ) ++ approvalSection(model) ++ List[Html[Msg]](
          div(
            h2("Snapshot"),
            p(
              text(
                "When the run suspends, its entire state is serialised here as JSON via the circe codecs. You can Save it, reload the page, Load it back, and approve the waiting nodes to finish the run."
              )
            ),
            textarea(
              attr("rows")     := "12",
              attr("readonly") := "readonly",
              style            := "width: 100%; font-family: monospace; box-sizing: border-box;",
              value            := model.snapshotText
            )(),
            div(
              button(onClick(Msg.SaveSnapshot))("Save to localStorage"),
              button(onClick(Msg.LoadSnapshot))("Load from localStorage")
            )
          )
        ))*
      ),
      div(
        style := "flex: 1 1 auto; min-width: 300px;"
      )(
        h1("Hedgehogs Suspendable Visitor Demo"),
        p(
          text(
            "This demonstrates concurrently running a graph of dependencies with the suspendable visitor. Each round node is an operation that takes 1 second to run, and each hexagonal node needs an approval: when the run reaches it, the whole run suspends into a snapshot until you approve it. An arrow represents dependency flow, the target needs the source to run before it can start."
          )
        ),
        p(
          Html.span(
            text("Each node is coloured according to state, it may be "),
            State.Pending.toHtml,
            text(", "),
            State.Running.toHtml,
            text(", "),
            State.Suspended.toHtml,
            text(" or "),
            State.Done.toHtml,
            text("; the log shows transitions as they happen, with a clock that keeps running across suspensions.")
          )
        ),
        div(
          style := "min-height: 400px;"
        )(
          h2("Graph"),
          div(
            style := "display: flex; justify-content: center;"
          )(
            model.graphviz
              .map { gv =>
                val renderGraph = model.graph.mapNode(_._1)
                Html
                  .raw("div")(gv.dot(Digraph(renderGraph), "svg_inline"))
                  .withKey(Some(renderGraph.hashCode().toString))
              }
              .getOrElse(div(text("Waiting for graphviz wasm to load")))
          )
        )
      ),
      div(
        style := "flex: 0 0 350px;"
      )(
        h2("Logs"),
        div(
          style := "height: 85vh; overflow: auto; display: flex; flex-direction: column-reverse;"
        )(
          model.messages.reverse*
        )
      )
    )

  def approvalSection(model: Model): List[Html[Msg]] =
    model.snapshot match
      case Some(snap) if !model.running =>
        List(
          div(
            h2("Waiting for approval"),
            div(
              waitingIds(snap).map(id => button(onClick(Msg.Approve(id)))(s"Approve $id"))*
            )
          )
        )
      case _                            => Nil

  def subscriptions(model: Model): Sub[IO, Msg]      =
    model.queue match
      case None        => Sub.None
      case Some(queue) => Sub.make("events", fs2.Stream.eval(queue.take).repeat)

enum Msg:
  // setup events
  case QueueCreated(queue: Queue[IO, Msg])
  case LoadGraphviz
  case GraphvizLoaded(g: Graphviz)
  // user interaction
  case Reroll
  case Start
  case NodeCount(n: Int)
  case EdgeCount(e: Int)
  case ApprovalCount(n: Int)
  case Approve(id: String)
  case ApplySnapshot
  case SaveSnapshot
  case LoadSnapshot
  // run lifecycle
  case RunCompleted(result: RunResult[String, Node, Node, String, Unit])
  case RunFailed(message: String)
  case SnapshotLoaded(text: String)
  // respond to user
  case Log(msg: Html[Msg])
  case UpdateNodeState(node: String, state: State)
  case SyncNodeColours
  case ClearLog
  //
  case NoOp
