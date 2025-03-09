package net.andimiller.hedgehogs

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

}
