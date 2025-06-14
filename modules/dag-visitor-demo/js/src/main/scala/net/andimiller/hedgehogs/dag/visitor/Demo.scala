package net.andimiller.hedgehogs.dag.visitor

import net.andimiller.hedgehogs.DataGraph
import cats.effect.IO
import cats.implicits.*
import tyrian.Html.*
import tyrian.SVG.*
import tyrian.*
import cats.effect.std.Queue
import cats.Show

import scala.scalajs.js.annotation.*
import scala.concurrent.duration.*
import java.time.{Instant, Duration}
import scala.util.Random
import net.andimiller.hedgehogs.Dag

enum Node:
  case WaitSeconds(n: FiniteDuration)
object Node:
  given Show[Node] = new Show[Node] {
    override def show(n: Node): String =
      n match
        case WaitSeconds(n) =>
          n.toString.replace(" ", "").replace("econds", "").replace("econd", "") // quickly make it shorter
  }

enum State:
  case Waiting, Started, Done
  def toColour: String  =
    this match
      case Waiting => "indianred"
      case Started => "coral"
      case Done    => "limegreen"
  def toHtml: Html[Msg] = Html.span(style := s"color:${this.toColour};")(this.toString)

case class Model(
    startTime: Instant,
    graph: DataGraph[String, (Node, State), Unit],
    messages: Vector[Html[Msg]],
    queue: Option[Queue[IO, (String, State)]],
    nodes: Int = 5,
    edges: Int = 5,
    graphviz: Option[Graphviz] = None
)

@JSExportTopLevel("TyrianApp")
object Demo extends TyrianIOApp[Msg, Model]:

  def makeProgram(nodes: Int, edges: Int): DataGraph[String, Node, Unit] = {
    val nodeGraph   = (('A' to 'Z') ++ ('a' to 'z'))
      .take(nodes)
      .foldLeft(
        DataGraph.empty[String, Node, Unit]
      ) { case (g, n) =>
        g.addNode(n.toString, Node.WaitSeconds(1.seconds))
      }
    val nodesVector = nodeGraph.nodes.toVector
    (0 to edges).foldLeft(nodeGraph) { case (g, _) =>
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
  }

  def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (
      Model(Instant.now(), makeProgram(5, 5).mapNode(n => (n, State.Waiting)), Vector(), None),
      Cmd.Batch(
        Cmd.Run(Queue.unbounded[IO, (String, State)].map(Msg.QueueCreated.apply)),
        Cmd.Emit(Msg.Reroll),
        Cmd.Emit(Msg.LoadGraphviz)
      )
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.NoOp                   => (model, Cmd.None)
    case Msg.Start                  =>
      val runner = new SimpleDagVisitor[IO, String, Node, Unit, Unit] {
        override def run(id: String, node: Node, inputs: Map[String, Unit]): IO[Unit] = {
          node match {
            case Node.WaitSeconds(n) =>
              model.queue.traverse(_.offer(id -> State.Started)) *> IO
                .sleep(n) *> model.queue.traverse(_.offer(id -> State.Done)).void
          }
        }
      }
      (
        model.copy(
          startTime = Instant.now(),
          graph = model.graph.mapNode { case (n, _) => n -> State.Waiting }
        ),
        Cmd.Batch(
          Cmd.Emit(Msg.ClearLog),
          Cmd.Run(DagVisitor.runConcurrent(runner)(model.graph.mapNode(_._1)).as(Msg.NoOp))
        )
      )
    case Msg.Reroll                 =>
      val newGraph = makeProgram(model.nodes, model.edges).mapNode(n => (n, State.Waiting))
      (
        model.copy(
          startTime = Instant.now(),
          graph = newGraph
        ),
        Cmd.Batch(
          Cmd.Emit(Msg.ClearLog),
          Cmd.Emit(
            Msg.Log(
              Html.span(
                text(
                  s"Graph has been regenerated with ${newGraph.nodes.size} nodes and ${newGraph.edges.size} edges, all nodes reset to "
                ),
                State.Waiting.toHtml
              )
            )
          )
        )
      )
    case Msg.LoadGraphviz           =>
      (
        model,
        Cmd.Run(GraphvizIO.load().map(Msg.GraphvizLoaded.apply))
      )
    case Msg.GraphvizLoaded(g)      =>
      (
        model.copy(graphviz = Some(g)),
        Cmd.Emit(Msg.Log(Html.span(text("Graphviz loaded"))))
      )
    case Msg.NodeCount(n)           =>
      (
        model.copy(
          nodes = n
        ),
        Cmd.Emit(Msg.Reroll)
      )
    case Msg.EdgeCount(e)           =>
      (
        model.copy(
          edges = e
        ),
        Cmd.Emit(Msg.Reroll)
      )
    case Msg.QueueCreated(q)        =>
      (model.copy(queue = Some(q)), Cmd.None)
    case Msg.UpdateNodeState(id, s) =>
      val now       = Instant.now()
      val ts        = Duration.between(model.startTime, now).getSeconds()
      val nextGraph = model.graph.addNode(id, model.graph.nodeMap(id)._1 -> s)
      (
        model.copy(
          graph = nextGraph
        ),
        Cmd.Batch(
          List(
            Msg.Log(Html.span(text(s"$id changed state to "), s.toHtml)).some,
            Option.when(nextGraph.nodeMap.values.map(_._2).forall(_ == State.Done))(
              Msg.Log(Html.span(text("Graph has is all "), State.Done.toHtml))
            )
          ).flatten.map(Cmd.Emit.apply)
        )
      )
    case Msg.Log(msg)               =>
      val now = Instant.now()
      val ts  = Duration.between(model.startTime, now).getSeconds()
      (
        model.copy(
          messages = model.messages.appended(Html.span(text(s"${ts}s "), msg))
        ),
        Cmd.None
      )
    case Msg.ClearLog               =>
      (model.copy(messages = Vector()), Cmd.None)

  def view(model: Model): Html[Msg] =
    div(
      h1("Hedgehogs Concurrent Visitor Demo"),
      p(
        text(
          "This demonstrates concurrently running a graph with dependencies, each Node represents an operation that takes 1 second to run, and an arrow represents dependency flow, the target needs the source to run before it can start."
        )
      ),
      p(
        Html.span(
          text("Each node is coloured according to state, it may be "),
          State.Waiting.toHtml,
          text(", "),
          State.Started.toHtml,
          text(" or "),
          State.Done.toHtml,
          text("; the log below shows transitions as they happen.")
        )
      ),
      div(
        cls := "row"
      )(
        div(
          style := "float: left; width: 400px;"
        )(
          h2("Controls"),
          label(
            attr("for")    := "nodes",
            style          := "display: inline-block; vertical-align: middle;"
          )(
            text("Nodes"),
            input(
              id           := "nodes",
              attr("type") := "range",
              attr("min")  := "2",
              value        := "5",
              attr("max")  := "50",
              style        := "display: inline-block; vertical-align: middle;",
              onChange(value => Msg.NodeCount(value.toInt))
            ),
            text(model.nodes.toString)
          ),
          hr,
          label(
            attr("for")    := "edges",
            style          := "display: inline-block; vertical-align: middle;"
          )(
            text("Edges"),
            input(
              id           := "edges",
              attr("type") := "range",
              attr("min")  := "2",
              value        := "10",
              attr("max")  := "100",
              style        := "display: inline-block; vertical-align: middle;",
              onChange(value => Msg.EdgeCount(value.toInt))
            ),
            text(model.edges.toString)
          ),
          hr,
          button(onClick(Msg.Reroll))("Reroll"),
          button(onClick(Msg.Start))("Start")
        ),
        div(
          style := "float: right; width: 400px; min-height: 400px;"
        )(
          h2("Graph"),
          model.graphviz
            .map { gv =>
              Html.raw("div")(gv.dot(Digraph(model.graph.mapNode(_._2)), "svg_inline"))
            }
            .getOrElse(div(text("Waiting for graphviz wasm to load")))
        )
      ),
      div(
        h2("Logs"),
        div(
          style := "height: 300px; overflow: auto; display: flex; flex-direction: column-reverse;"
        )(
          model.messages.reverse*
        )
      )
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    model.queue match
      case None        => Sub.None
      case Some(queue) => Sub.make("events", fs2.Stream.eval(queue.take).map(Msg.UpdateNodeState.apply).repeat)

enum Msg:
  // setup events
  case QueueCreated(queue: Queue[IO, (String, State)])
  case LoadGraphviz
  case GraphvizLoaded(g: Graphviz)
  // user interaction
  case Reroll
  case Start
  case NodeCount(n: Int)
  case EdgeCount(e: Int)
  // respond to user
  case Log(msg: Html[Msg])
  case UpdateNodeState(node: String, state: State)
  case ClearLog
  //
  case NoOp
