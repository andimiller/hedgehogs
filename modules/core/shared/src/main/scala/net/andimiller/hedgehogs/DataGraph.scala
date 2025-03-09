package net.andimiller.hedgehogs

import cats.kernel.Monoid

trait DataGraph[Id, NodeData, EdgeData] extends SimpleGraph[Id] { self =>
  // data
  def nodeMap: Map[Id, NodeData]
  def edgeMap: Map[(Id, Id), EdgeData]

  // extra graph operations
  def outgoingEdges(id: Id): Map[Id, EdgeData]

  // extra modify operations
  def addNode(id: Id, data: NodeData): DataGraph[Id, NodeData, EdgeData]
  def addEdge(from: Id, to: Id, data: EdgeData): DataGraph[Id, NodeData, EdgeData]

  // modifier type overrides
  def addNode(id: Id): DataGraph[Id, NodeData, EdgeData]
  def removeNode(id: Id): DataGraph[Id, NodeData, EdgeData]
  def addEdge(from: Id, to: Id): DataGraph[Id, NodeData, EdgeData]
  def removeEdge(from: Id, to: Id): DataGraph[Id, NodeData, EdgeData]

  def map[Id2](f: Id => Id2): DataGraph[Id2, NodeData, EdgeData]
  def mapNode[NodeData2: Monoid](f: NodeData => NodeData2): DataGraph[Id, NodeData2, EdgeData]
  def mapEdge[EdgeData2: Monoid](f: EdgeData => EdgeData2): DataGraph[Id, NodeData, EdgeData2]
}

object DataGraph {
  def empty[Id, NodeData: Monoid, EdgeData: Monoid]: DataGraph[Id, NodeData, EdgeData] =
    new AdjacencyListDataGraph[Id, NodeData, EdgeData](Map.empty, Map.empty)
}

case class AdjacencyListDataGraph[Id, NodeData: Monoid, EdgeData: Monoid](
    nodeMap: Map[Id, NodeData],
    edgeMap: Map[(Id, Id), EdgeData]
) extends DataGraph[Id, NodeData, EdgeData] {

  override def nodes: Set[Id] = nodeMap.keySet

  override def edges: Set[(Id, Id)] = edgeMap.keySet

  override def addNode(id: Id): DataGraph[Id, NodeData, EdgeData] =
    copy(
      nodeMap = nodeMap.updated(id, Monoid[NodeData].empty)
    )

  override def addNode(id: Id, data: NodeData): DataGraph[Id, NodeData, EdgeData] =
    copy(
      nodeMap = nodeMap.updated(id, data)
    )

  override def removeNode(id: Id): DataGraph[Id, NodeData, EdgeData] =
    copy(
      nodeMap = nodeMap.removed(id),
      edgeMap = edgeMap.filterNot { case ((from, to), _) =>
        from == id || to == id
      }
    )

  override def addEdge(from: Id, to: Id): DataGraph[Id, NodeData, EdgeData] =
    copy(
      edgeMap = edgeMap.updated((from, to), Monoid[EdgeData].empty)
    )

  override def addEdge(from: Id, to: Id, data: EdgeData): DataGraph[Id, NodeData, EdgeData] =
    copy(
      edgeMap = edgeMap.updated((from, to), data)
    )

  override def removeEdge(from: Id, to: Id): DataGraph[Id, NodeData, EdgeData] =
    copy(
      edgeMap = edgeMap.removed((from, to))
    )

  override def outgoing(id: Id): Set[Id]                                  =
    edges.collect { case (from, to) if from == id => to }

  override def outgoingEdges(id: Id): Map[Id, EdgeData]                   =
    edgeMap.collect { case ((from, to), data) if from == id => to -> data }

  override def inbound(id: Id): Set[Id]                                   =
    edges.collect { case (from, to) if to == id => from }

  override def map[Id2](f: Id => Id2): DataGraph[Id2, NodeData, EdgeData] =
    new AdjacencyListDataGraph[Id2, NodeData, EdgeData](
      nodeMap.map { case (id, data) =>
        f(id) -> data
      },
      edgeMap.map { case ((from, to), data) =>
        (f(from), f(to)) -> data
      }
    )

  override def mapNode[NodeData2: Monoid](f: NodeData => NodeData2): DataGraph[Id, NodeData2, EdgeData] =
    new AdjacencyListDataGraph[Id, NodeData2, EdgeData](
      nodeMap.view.mapValues(f).toMap,
      edgeMap
    )

  override def mapEdge[EdgeData2: Monoid](f: EdgeData => EdgeData2): DataGraph[Id, NodeData, EdgeData2] =
    new AdjacencyListDataGraph[Id, NodeData, EdgeData2](
      nodeMap,
      edgeMap.view.mapValues(f).toMap
    )
}
