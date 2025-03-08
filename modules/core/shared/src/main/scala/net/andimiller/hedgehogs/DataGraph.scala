package net.andimiller.hedgehogs

trait DataGraph[Id, NodeData, EdgeData] extends SimpleGraph[Id] {
  def nodeMap: Map[Id, NodeData]
  def edgeMap: Map[(Id, Id), EdgeData]
}

case class AdjacencyListDataGraph[Id, NodeData, EdgeData](nodeMap: Map[Id, NodeData], edgeMap: Map[(Id, Id), EdgeData]) extends DataGraph[Id, NodeData, EdgeData] {

  override def nodes: Set[Id] = nodeMap.keySet

  override def edges: Set[(Id, Id)] = edgeMap.keySet

  override def addNode(id: Id): SimpleGraph[Id] =
    copy(
      
    )

  override def removeNode(id: Id): SimpleGraph[Id] = ???

  override def addEdge(from: Id, to: Id): SimpleGraph[Id] = ???

  override def removeEdge(from: Id, to: Id): SimpleGraph[Id] = ???
}