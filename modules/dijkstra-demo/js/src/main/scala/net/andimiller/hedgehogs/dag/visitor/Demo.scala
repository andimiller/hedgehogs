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

case class Model(
    startTime: Instant,
    graph: DataGraph[String, Option[Int], Unit],
    currentNode: String,
    paths: List[(Int, List[String])] = List.empty,
    visited: Set[String] = Set.empty,
    messages: Vector[Html[Msg]] = Vector.empty,
    ticking: Boolean = false,
    nodes: Int = 5,
    edges: Int = 5,
    graphviz: Option[Graphviz] = None
)

@JSExportTopLevel("TyrianApp")
object Demo extends TyrianIOApp[Msg, Model]:

  val nodeNames: Seq[String] = (('A' to 'Z') ++ ('a' to 'z')).map(_.toString)

  def makeGraph(nodes: Int, edges: Int): (DataGraph[String, Option[Int], Unit], String) = {
    val nodeGraph   = nodeNames
      .take(nodes)
      .foldLeft(
        DataGraph.empty[String, Option[Int], Unit]
      ) { case (g, n) =>
        g.addNode(n.toString, None)
      }
    val start = Random.shuffle(nodeGraph.nodes).head
    val nodeGraph2 = nodeGraph.addNode(start, Some(0))
    val nodesVector = nodeGraph.nodes.toVector
    (0 to edges).foldLeft(nodeGraph) { case (g, _) =>
      val from = nodesVector(Random.between(0, g.nodes.size.toInt))
      val to   = nodesVector(Random.between(0, g.nodes.size.toInt))
      if (from != to) {
        g.addEdge(from, to, ())
      } else {
        g
      }
    } -> start
  }

  def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = {
    val (graph, startId) = makeGraph(5, 5)
    (
      Model(
        startTime = Instant.now(),
        graph = graph,
        currentNode = startId,
        paths = List(0 -> List(startId)),
      ),
      Cmd.Batch(
        Cmd.Emit(Msg.Reroll),
        Cmd.Emit(Msg.LoadGraphviz)
      )
    )
  }

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.NoOp                   => (model, Cmd.None)
    case Msg.Reroll                 =>
      val (newGraph, newStart) = makeGraph(model.nodes, model.edges)
      (
        model.copy(
          startTime = Instant.now(),
          graph = newGraph,
          currentNode = newStart,
          paths = List(0 -> List(newStart)),
          visited = Set.empty,
          ticking = false,
        ),
        Cmd.Batch(
          Cmd.Emit(Msg.ClearLog),
          Cmd.Emit(
            Msg.Log(
              Html.span(
                text(
                  s"Graph has been regenerated with ${newGraph.nodes.size} nodes and ${newGraph.edges.size} edges, new starting point is $newStart" 
                )
              )
            )
          ),
          Cmd.None
        )
      )
    case Msg.Start =>
      (
        model.copy(ticking = true),
        Cmd.None
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
    case Msg.ClockTick if model.ticking =>
      (model, Cmd.Emit(Msg.TickDijkstra))
    case Msg.ClockTick =>
      (model, Cmd.None)
    case Msg.TickDijkstra =>
      model.paths match {
        case (distance, path) :: otherPaths => {
          path match {
            case head :: _ =>
              val extraPaths =
                model.graph.outgoingEdges(head).toList.flatMap {
                  case (key, _) if model.visited.contains(key) => List.empty
                  case (key, _) =>
                    List((distance + 1, key :: path))
                }
              val sortedPaths = (extraPaths ++ otherPaths).sortBy(_._1)
              val sortedPathsWithThis = (extraPaths ++ model.paths).sortBy(_._1)
              val distances = sortedPathsWithThis.groupBy { case (_, path) =>
                path.head
              }.view.mapValues(_.min)
              val nextGraph = distances.foldLeft(model.graph) { case (g, (id, (d, _))) =>
                g.addNode(id, Some(d))
              }
              (
                model.copy(
                  graph = nextGraph,
                  visited = model.visited + head,
                  currentNode = head,
                  paths = sortedPaths,
                ),
                Cmd.Emit(Msg.Log(Html.span(s"Visited $head neighbours, added ${extraPaths.size} new routes")))
              )
            case _ => (model, Cmd.None)
          }
        }
        case Nil => (model.copy(ticking = false, currentNode="None"), Cmd.Emit(Msg.Log(Html.span(s"Dijkstra Done"))))
      }


  def view(model: Model): Html[Msg] =
    div(
      h1("Hedgehogs Dijkstra Demo"),
      p(
        text(
          "This demonstrates calculating dijkstra across a graph."
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
          button(onClick(Msg.Start))("Start"),
          button(onClick(Msg.Reroll))("Reroll"),
          button(onClick(Msg.TickDijkstra))("Tick")
        ),
        div(
          style := "float: right; width: 400px; min-height: 400px;"
        )(
          h2("Graph"),
          model.graphviz
            .map { gv =>
              Html.raw("div")(gv.fdp(Digraph(model.currentNode, model.graph), "svg_inline")).withKey(Some(model.graph.hashCode().toString))
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
    Sub.every[IO](1.second, "tick").map(_ => Msg.ClockTick)

enum Msg:
  // setup events
  case LoadGraphviz
  case GraphvizLoaded(g: Graphviz)
  // user interaction
  case Start
  case TickDijkstra
  case Reroll
  case NodeCount(n: Int)
  case EdgeCount(e: Int)
  // respond to user
  case Log(msg: Html[Msg])
  case ClearLog
  //
  case ClockTick
  case NoOp
