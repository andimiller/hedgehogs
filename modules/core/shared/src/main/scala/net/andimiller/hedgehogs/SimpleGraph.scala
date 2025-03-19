package net.andimiller.hedgehogs

import cats.Monoid
import cats.implicits._

trait SimpleGraph[Id] {
  // data
  def nodes: Set[Id]
  def edges: Set[(Id, Id)]

  // graph operations
  def outgoing(id: Id): Set[Id]
  def inbound(id: Id): Set[Id]

  def reverse: SimpleGraph[Id]

  // modifiers
  def addNode(id: Id): SimpleGraph[Id]
  def removeNode(id: Id): SimpleGraph[Id]
  def addEdge(from: Id, to: Id): SimpleGraph[Id]
  def removeEdge(from: Id, to: Id): SimpleGraph[Id]

  def map[Id2](f: Id => Id2): SimpleGraph[Id2]
}

case class AdjacencyListSimpleGraph[Id](nodes: Set[Id], edges: Set[(Id, Id)]) extends SimpleGraph[Id] {

  override def addNode(id: Id): AdjacencyListSimpleGraph[Id] =
    copy(
      nodes = nodes + id
    )

  override def removeNode(id: Id): AdjacencyListSimpleGraph[Id] = copy(
    nodes = nodes - id,
    edges = edges.filter {
      case (from, to) if Set(from, to) contains id => false
      case _                                       => true
    }
  )

  override def addEdge(from: Id, to: Id): AdjacencyListSimpleGraph[Id] = copy(
    edges = edges + ((from, to))
  )

  override def removeEdge(from: Id, to: Id): AdjacencyListSimpleGraph[Id] = copy(
    edges = edges - ((from, to))
  )

  override def outgoing(id: Id): Set[Id]                =
    edges.collect { case (from, to) if from == id => to }

  override def inbound(id: Id): Set[Id]                 =
    edges.collect { case (from, to) if to == id => from }

  override def map[Id2](f: Id => Id2): SimpleGraph[Id2] =
    new AdjacencyListSimpleGraph[Id2](
      nodes.map(f),
      edges.map { case (from, to) =>
        f(from) -> f(to)
      }
    )

  override def reverse: SimpleGraph[Id] =
    copy(
      edges = edges.map(_.swap)
    )
}

object SimpleGraph {
  def empty[Id] = new AdjacencyListSimpleGraph[Id](Set.empty, Set.empty)

  implicit def monoid[Id]: Monoid[SimpleGraph[Id]] = new Monoid[SimpleGraph[Id]] {
    override def empty: SimpleGraph[Id] = AdjacencyListSimpleGraph[Id](Set.empty, Set.empty)

    override def combine(x: SimpleGraph[Id], y: SimpleGraph[Id]): SimpleGraph[Id] =
      AdjacencyListSimpleGraph[Id](x.nodes ++ y.nodes, x.edges ++ y.edges)
  }
}
