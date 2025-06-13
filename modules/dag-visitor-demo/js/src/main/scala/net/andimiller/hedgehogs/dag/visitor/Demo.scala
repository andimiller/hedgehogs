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
    edges: Int = 5
)

@JSExportTopLevel("TyrianApp")
object Counter extends TyrianIOApp[Msg, Model]:

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
        Cmd.Emit(Msg.Reroll)
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

  // keep track of svg sizing
  val centerX = 200.0
  val centerY = 200.0
  val radius  = 400.0 / 3.0

  def view(model: Model): Html[Msg] =
    val nodePositions: Map[String, (Double, Double)] = {
      val n = model.graph.nodes.size
      if (n == 0) Map.empty
      else {
        val step = (2 * math.Pi) / n
        model.graph.nodes.zipWithIndex.map { case (node, idx) =>
          val angle = idx * step
          val x     = centerX + radius * math.cos(angle)
          val y     = centerY + radius * math.sin(angle)
          node -> (x, y)
        }.toMap
      }
    }

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
          style := "float: right;"
        )(
          h2("Graph"),
          svg(viewBox := "0, 0, 400, 400", width := "400px")(
            List(
              marker(
                attr("markerWidth")  := "25",
                attr("markerHeight") := "25",
                viewBox              := "0 0 50 50",
                attr("refX")         := "25",
                attr("refY")         := "25",
                attr("orient")       := "auto-start-reverse",
                id                   := "arrowhead",
                stroke               := "grey",
                fill                 := "grey",
                style                := "stroke-width: 2px; stroke grey;"
              )(
                path(
                  d                  := "M 0 0 L 50 25 L 0 50 z"
                )
              )
            ) ++ model.graph.edges.toList.map { case (from, to) =>
              val (x1d, y1d) = nodePositions(from)
              val (x2d, y2d) = nodePositions(to)
              val (xm, ym)   = ((x1d + x2d) / 2, (y1d + y2d) / 2)
              polyline(
                points               := s"$x1d,$y1d $xm,$ym $x2d,$y2d",
                stroke               := "grey",
                attr("marker-mid")   := "url(#arrowhead)",
                attr("marker-end")   := "url(#arrowhead)",
                attr("marker-start") := "url(#arrowhead)"
              )
            } ++ model.graph.nodeMap.toList.map { case (id, (node, state)) =>
              val (x, y) = nodePositions(id)
              g(
                circle(
                  cx     := x.toString,
                  cy     := y.toString,
                  r      := "20",
                  fill   := state.toColour,
                  stroke := "grey"
                ),
                tag("text")(
                  attr("x")                 := x,
                  attr("y")                 := y,
                  style                     := "font-family: Sans,Arial;",
                  attr("text-anchor")       := "middle",
                  attr("dominant-baseline") := "middle"
                )(text(show"$id"))
              )
            }
          )
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
  case Reroll
  case Start
  case NoOp
  case QueueCreated(queue: Queue[IO, (String, State)])
  case UpdateNodeState(node: String, state: State)
  case NodeCount(n: Int)
  case EdgeCount(e: Int)
  case Log(msg: Html[Msg])
  case ClearLog
