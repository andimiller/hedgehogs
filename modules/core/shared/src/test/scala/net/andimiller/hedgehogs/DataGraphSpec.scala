package net.andimiller.hedgehogs

import cats.data.NonEmptyList
import cats.implicits.catsSyntaxValidatedId

class DataGraphSpec extends munit.FunSuite {

  test("Build a graph from an empty graph") {
    assertEquals(
      obtained = DataGraph
        .empty[Int, String, Double]
        .addNode(1, "Node One")
        .addNode(2, "Node Two")
        .addNode(3, "Node Three")
        .addEdge(1, 2, 4.5)
        .addEdge(2, 3, 7.2),
      expected = new AdjacencyListDataGraph[Int, String, Double](
        nodeMap = Map(
          1 -> "Node One",
          2 -> "Node Two",
          3 -> "Node Three"
        ),
        edgeMap = Map(
          (1, 2) -> 4.5,
          (2, 3) -> 7.2
        )
      )
    )
  }

  test("Find neighbours") {
    val graph = DataGraph
      .empty[Int, String, Double]
      .addNode(1, "Node One")
      .addNode(2, "Node Two")
      .addNode(3, "Node Three")
      .addEdge(1, 2, 4.5)
      .addEdge(2, 3, 7.2)
    assertEquals(
      obtained = graph.outgoing(2),
      expected = Set(3)
    )
    assertEquals(
      obtained = graph.outgoingEdges(2),
      expected = Map(3 -> 7.2)
    )
  }

  test("Map methods") {
    val graph = DataGraph
      .empty[Int, String, Double]
      .addNode(1, "Node One")
      .addNode(2, "Node Two")
      .addNode(3, "Node Three")
      .addEdge(1, 2, 4.5)
      .addEdge(2, 3, 7.2)
      .map(_.toString)
      .mapNode(_.reverse)
      .mapEdge(BigDecimal.decimal)
    assertEquals(
      obtained = graph,
      expected = new AdjacencyListDataGraph[String, String, BigDecimal](
        nodeMap = Map(
          "1" -> "Node One".reverse,
          "2" -> "Node Two".reverse,
          "3" -> "Node Three".reverse
        ),
        edgeMap = Map(
          ("1", "2") -> BigDecimal("4.5"),
          ("2", "3") -> BigDecimal("7.2")
        )
      )
    )
  }

  test("Validate a graph") {
    val graph = DataGraph
      .empty[String, Int, Unit] // the number in each node should be the number of inbounds
      .addNode("good0", 0)
      .addNode("bad0", 0)
      .addNode("good1", 1)
      .addNode("bad1", 1)
      .addNode("good2", 2)
      .addNode("bad2", 2)
      .addEdge("good0", "bad0", ())
      .addEdge("bad1", "good1", ())
      .addEdge("good0", "bad1", ())
      .addEdge("bad0", "bad1", ())
      .addEdge("good0", "good2", ())
      .addEdge("bad1", "good2", ())
      .addEdge("bad0", "bad2", ())

    val result = graph
      .traverseNode { case (k, v) =>
        val inbounds = graph.inbound(k).size
        if (inbounds == v)
          v.validNel
        else
          s"node $k had $inbounds inbound edges, expected $v".invalidNel
      }
      .leftMap(_.sorted)

    assertEquals(
      result,
      NonEmptyList
        .of(
          "node bad0 had 1 inbound edges, expected 0",
          "node bad1 had 2 inbound edges, expected 1",
          "node bad2 had 1 inbound edges, expected 2"
        )
        .invalid
    )
  }

}
