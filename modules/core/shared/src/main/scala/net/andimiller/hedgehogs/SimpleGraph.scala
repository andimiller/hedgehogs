package net.andimiller.hedgehogs

import cats.Monoid

// basic graph that has nodes and edges
trait SimpleGraph[Id] {
  // data
  def nodes: Set[Id]

  def edges: Set[(Id, Id)]

  // operations
  def addNode(id: Id): SimpleGraph[Id]

  def removeNode(id: Id): SimpleGraph[Id]

  def addEdge(from: Id, to: Id): SimpleGraph[Id]

  def removeEdge(from: Id, to: Id): SimpleGraph[Id]
}

case class AdjacencyListSimpleGraph[Id](nodes: Set[Id], edges: Set[(Id, Id)]) extends SimpleGraph[Id] {

  override def addNode(id: Id): SimpleGraph[Id] =
    copy(
      nodes = nodes + id
    )

  override def removeNode(id: Id): SimpleGraph[Id] = copy(
    nodes = nodes - id,
    edges = edges.filter {
      case (from, to) if Set(from, to) contains id => false
      case _ => true
    }
  )

  override def addEdge(from: Id, to: Id): SimpleGraph[Id] = copy(
    edges = edges + ((from, to))
  )

  override def removeEdge(from: Id, to: Id): SimpleGraph[Id] = copy(
    edges = edges - ((from, to))
  )
}

object SimpleGraph {
  implicit def monoid[Id]: Monoid[SimpleGraph[Id]] = new Monoid[SimpleGraph[Id]] {
    override def empty: SimpleGraph[Id] = AdjacencyListSimpleGraph[Id](Set.empty, Set.empty)

    override def combine(x: SimpleGraph[Id], y: SimpleGraph[Id]): SimpleGraph[Id] =
      AdjacencyListSimpleGraph[Id](x.nodes ++ y.nodes, x.edges ++ y.edges)
  }
}
