package net.andimiller.hedgehogs

import cats.implicits._
import cats.data.ValidatedNel

case class Edge[NodeId, Num](from: NodeId, to: NodeId, weight: Num)

case class Node[Id, Data](id: Id, data: Data)

case class Graph[Id, Data, Distance](nodes: Map[Id, Data], edges: Map[Id, Vector[(Id, Distance)]]) {
  def neighbours(id: Id): Vector[(Id, Distance)] = edges.getOrElse(id, Vector.empty)
  def removed(id: Id): Graph[Id, Data, Distance] =
    copy(
      nodes = nodes.removed(id),
      edges = edges.removed(id).view.mapValues(_.filter(_._1 != id)).toMap
    )
}

object Graph {
  def fromIterables[Id, Data, Distance](
      nodes: Iterable[Node[Id, Data]],
      edges: Iterable[Edge[Id, Distance]],
      bidirectional: Boolean
  ): ValidatedNel[String, Graph[Id, Data, Distance]] = {
    nodes.toVector
      .groupBy(_.id)
      .toVector
      .traverse { case (k, vs) =>
        if (vs.size > 1)
          s"$k has ${vs.size} nodes, required at most 1".invalidNel
        else
          (k -> vs.head.data).validNel
      }
      .map(_.toMap)
      .andThen { (nodeMap: Map[Id, Data]) =>
        val nodeExists: Id => ValidatedNel[String, Unit] =
          id => if (!nodeMap.contains(id)) s"$id is not a known node".invalidNel else ().validNel
        edges.toVector
          .groupBy(_.from)
          .toVector
          .traverse {
            case (k, _) if !nodeMap.contains(k) => s"$k is not a known node".invalidNel
            case (k, vs)                        => vs.map(_.to).traverse(nodeExists).as(k -> vs.map(v => v.to -> v.weight))
          }
          .map { edgeMap =>
            val extras =
              if (bidirectional) edges.toVector.groupBy(_.to).map(_.map(_.map(v => v.from -> v.weight))) else Map.empty
            Graph(nodeMap, (edgeMap ++ extras).toMap)
          }
      }
  }

}
