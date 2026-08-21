package net.andimiller.hedgehogs.dag.visitor.circe

import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax._
import net.andimiller.hedgehogs.AdjacencyListDataGraph
import net.andimiller.hedgehogs.dag.visitor.{DagSnapshot, NodeState}
import net.andimiller.hedgehogs.dag.visitor.DagVisitor.RunMode

/** Codecs for persisting a suspended run.
  *
  * The JSON shape is:
  * {{{
  * {
  *   "version": 1,
  *   "direction": "flow",
  *   "nodes": [ {"id": ..., "state": {"type": "waiting", "input": ..., "handle": ...}}, ... ],
  *   "edges": [ {"from": ..., "to": ..., "data": ...}, ... ]
  * }
  * }}}
  *
  * Ids are encoded as values (not object keys) so they only need an `Encoder`/`Decoder`, not a
  * `KeyEncoder`/`KeyDecoder`.
  */
object DagSnapshotCodecs {

  implicit val runModeEncoder: Encoder[RunMode] = Encoder[String].contramap {
    case RunMode.Flow       => "flow"
    case RunMode.Dependency => "dependency"
  }

  implicit val runModeDecoder: Decoder[RunMode] = Decoder[String].emap {
    case "flow"       => Right(RunMode.Flow)
    case "dependency" => Right(RunMode.Dependency)
    case other        => Left(s"Unknown RunMode: $other")
  }

  implicit def nodeStateEncoder[In: Encoder, Out: Encoder, Wait: Encoder]: Encoder[NodeState[In, Out, Wait]] =
    Encoder.instance {
      case NodeState.Pending(input)         =>
        Json.obj("type" := "pending", "input" := (input: In))
      case NodeState.Waiting(input, handle) =>
        Json.obj("type" := "waiting", "input" := (input: In), "handle" := (handle: Wait))
      case NodeState.Done(output)           =>
        Json.obj("type" := "done", "output" := (output: Out))
    }

  implicit def nodeStateDecoder[In: Decoder, Out: Decoder, Wait: Decoder]: Decoder[NodeState[In, Out, Wait]] =
    Decoder.instance { c =>
      c.get[String]("type").flatMap {
        case "pending" => c.get[In]("input").map(NodeState.Pending(_))
        case "waiting" => (c.get[In]("input"), c.get[Wait]("handle")).mapN(NodeState.Waiting(_, _))
        case "done"    => c.get[Out]("output").map(NodeState.Done(_))
        case other     => Left(DecodingFailure(s"Unknown NodeState type: $other", c.history))
      }
    }

  implicit def dagSnapshotEncoder[Id: Encoder, In: Encoder, Out: Encoder, Wait: Encoder, Edge: Encoder]
      : Encoder[DagSnapshot[Id, In, Out, Wait, Edge]] =
    Encoder.instance { snapshot =>
      Json.obj(
        "version"   := snapshot.version,
        "direction" := snapshot.direction,
        "nodes"     := snapshot.graph.nodeMap.toList.map { case (id, state) =>
          Json.obj("id" := id, "state" := state)
        },
        "edges"     := snapshot.graph.edgeMap.toList.map { case ((from, to), data) =>
          Json.obj("from" := from, "to" := to, "data" := data)
        }
      )
    }

  implicit def dagSnapshotDecoder[Id: Decoder, In: Decoder, Out: Decoder, Wait: Decoder, Edge: Decoder]
      : Decoder[DagSnapshot[Id, In, Out, Wait, Edge]] = {
    val nodeEntry: Decoder[(Id, NodeState[In, Out, Wait])] =
      Decoder.instance { c =>
        (c.get[Id]("id"), c.get[NodeState[In, Out, Wait]]("state")).tupled
      }
    val edgeEntry: Decoder[((Id, Id), Edge)]               =
      Decoder.instance { c =>
        (c.get[Id]("from"), c.get[Id]("to"), c.get[Edge]("data")).mapN { case (from, to, data) =>
          ((from, to), data)
        }
      }
    Decoder.instance { c =>
      (
        c.get[Int]("version"),
        c.get[RunMode]("direction"),
        c.get[List[(Id, NodeState[In, Out, Wait])]]("nodes")(Decoder.decodeList(nodeEntry)),
        c.get[List[((Id, Id), Edge)]]("edges")(Decoder.decodeList(edgeEntry))
      ).mapN { case (version, direction, nodes, edges) =>
        DagSnapshot(
          AdjacencyListDataGraph(nodes.toMap, edges.toMap),
          direction,
          version
        )
      }
    }
  }
}
