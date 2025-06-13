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
  def toColour: String =
    this match
      case Waiting => "indianred"
      case Started => "coral"
      case Done    => "limegreen"

case class Model(
    startTime: Instant,
    graph: DataGraph[String, (Node, State), Unit],
    messages: Vector[String],
    queue: Option[Queue[IO, (String, State)]]
)

@JSExportTopLevel("TyrianApp")
object Counter extends TyrianIOApp[Msg, Model]:

  val program =
    DataGraph
      .empty[String, Node, Unit]
      .addNode("A", Node.WaitSeconds(1.seconds))
      .addNode("B", Node.WaitSeconds(5.seconds))
      .addNode("C", Node.WaitSeconds(1.seconds))
      .addNode("D", Node.WaitSeconds(2.seconds))
      .addNode("E", Node.WaitSeconds(3.seconds))
      .addNode("F", Node.WaitSeconds(4.seconds))
      .addNode("G", Node.WaitSeconds(5.seconds))
      .addEdge("A", "C", ())
      .addEdge("B", "C", ())
      .addEdge("D", "C", ())
      .addEdge("E", "C", ())
      .addEdge("F", "D", ())
      .addEdge("G", "A", ())

  def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    (
      Model(Instant.now(), program.mapNode(n => (n, State.Waiting)), Vector(), None),
      Cmd.Run(Queue.unbounded[IO, (String, State)].map(Msg.QueueCreated.apply))
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
      (model.copy(startTime = Instant.now()), Cmd.Run(DagVisitor.runConcurrent(runner)(program).as(Msg.NoOp)))
    case Msg.QueueCreated(q)        =>
      (model.copy(queue = Some(q)), Cmd.None)
    case Msg.UpdateNodeState(id, s) =>
      val now = Instant.now()
      val ts  = Duration.between(model.startTime, now).getSeconds()
      (
        model.copy(
          graph = model.graph.addNode(id, model.graph.nodeMap(id)._1 -> s),
          messages = model.messages.appended(s"${ts}s - $id changed state to $s")
        ),
        Cmd.None
      )

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
      button(onClick(Msg.Start))("Start"),
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
            style                := "stroke-width: 2px; stroke black;"
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
            stroke               := "black",
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
              stroke := "black"
            ),
            tag("text")(
              attr("x")                 := x,
              attr("y")                 := y,
              style                     := "font-family: Sans,Arial;",
              attr("text-anchor")       := "middle",
              attr("dominant-baseline") := "middle"
            )(text(show"$id $node"))
          )
        }
      ),
      div(
        model.messages.map(p)*
      )
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    model.queue match
      case None        => Sub.None
      case Some(queue) => Sub.make("events", fs2.Stream.eval(queue.take).map(Msg.UpdateNodeState.apply).repeat)

enum Msg:
  case Start
  case NoOp
  case QueueCreated(queue: Queue[IO, (String, State)])
  case UpdateNodeState(node: String, state: State)
