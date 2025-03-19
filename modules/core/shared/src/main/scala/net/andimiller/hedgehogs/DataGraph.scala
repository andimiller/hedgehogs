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
  def reverse: DataGraph[Id, NodeData, EdgeData]

  def map[Id2](f: Id => Id2): DataGraph[Id2, NodeData, EdgeData]
  def mapNode[NodeData2](f: NodeData => NodeData2): DataGraph[Id, NodeData2, EdgeData]
  def mapEdge[EdgeData2](f: EdgeData => EdgeData2): DataGraph[Id, NodeData, EdgeData2]
}

object DataGraph {
  def empty[Id, NodeData, EdgeData]: DataGraph[Id, NodeData, EdgeData] =
    new AdjacencyListDataGraph[Id, NodeData, EdgeData](Map.empty, Map.empty)

  implicit def monoid[Id, NodeData, EdgeData]: Monoid[DataGraph[Id, NodeData, EdgeData]] =
    new Monoid[DataGraph[Id, NodeData, EdgeData]] {
      override def empty: DataGraph[Id, NodeData, EdgeData] = DataGraph.empty[Id, NodeData, EdgeData]

      override def combine(
          x: DataGraph[Id, NodeData, EdgeData],
          y: DataGraph[Id, NodeData, EdgeData]
      ): DataGraph[Id, NodeData, EdgeData] =
        new AdjacencyListDataGraph[Id, NodeData, EdgeData](
          x.nodeMap ++ y.nodeMap,
          x.edgeMap ++ y.edgeMap
        )
    }
}

case class AdjacencyListDataGraph[Id, NodeData, EdgeData](
    nodeMap: Map[Id, NodeData],
    edgeMap: Map[(Id, Id), EdgeData],
    emptyNode: Option[NodeData] = None,
    emptyEdge: Option[EdgeData] = None
) extends DataGraph[Id, NodeData, EdgeData] {

  private lazy val emptyNodeOrThrow: NodeData = emptyNode.getOrElse(
    throw new Exception(
      "No default emptyNode available, please supply one when making the type if you want to add with defaults"
    )
  )
  private lazy val emptyEdgeOrThrow: EdgeData = emptyEdge.getOrElse(
    throw new Exception(
      "No default emptyEdge available, please supply one when making the type if you want to add with defaults"
    )
  )

  override def nodes: Set[Id] = nodeMap.keySet

  override def edges: Set[(Id, Id)] = edgeMap.keySet

  override def addNode(id: Id): DataGraph[Id, NodeData, EdgeData] =
    copy(
      nodeMap = nodeMap.updated(id, emptyNodeOrThrow)
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
      edgeMap = edgeMap.updated((from, to), emptyEdgeOrThrow)
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

  override def mapNode[NodeData2](f: NodeData => NodeData2): DataGraph[Id, NodeData2, EdgeData] =
    new AdjacencyListDataGraph[Id, NodeData2, EdgeData](
      nodeMap.view.mapValues(f).toMap,
      edgeMap
    )

  override def mapEdge[EdgeData2](f: EdgeData => EdgeData2): DataGraph[Id, NodeData, EdgeData2] =
    new AdjacencyListDataGraph[Id, NodeData, EdgeData2](
      nodeMap,
      edgeMap.view.mapValues(f).toMap
    )

  override def reverse: DataGraph[Id, NodeData, EdgeData] =
    copy(
      edgeMap = edgeMap.map { case (tuple, data) => tuple.swap -> data }
    )
}
