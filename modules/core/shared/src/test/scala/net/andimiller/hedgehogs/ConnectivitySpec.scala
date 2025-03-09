package net.andimiller.hedgehogs

class ConnectivitySpec extends munit.FunSuite {

  test("Should work on an empty graph") {
    assertEquals(
      obtained = Connectivity
        .countDisconnectedSubgraphs(
          SimpleGraph.empty[Int]
        )
        .value,
      expected = 0
    )
  }

  test("Should work in a graph that's a DAG but all connected") {
    assertEquals(
      obtained = Connectivity
        .countDisconnectedSubgraphs(
          SimpleGraph
            .empty[Int]
            .addNode(1)
            .addNode(2)
            .addNode(3)
            .addEdge(1, 2)
            .addEdge(2, 3)
        )
        .value,
      expected = 1
    )
  }

  test("Should work in a graph that's got two groups") {
    assertEquals(
      obtained = Connectivity
        .countDisconnectedSubgraphs(
          SimpleGraph
            .empty[Int]
            .addNode(1)
            .addNode(2)
            .addNode(3)
            .addEdge(1, 2)
            .addEdge(2, 1)
        )
        .value,
      expected = 2
    )
  }

  test("Should work on a load of disconnected nodes") {
    assertEquals(
      obtained = Connectivity
        .countDisconnectedSubgraphs(
          SimpleGraph
            .empty[Int]
            .addNode(1)
            .addNode(2)
            .addNode(3)
            .addNode(4)
            .addNode(5)
        )
        .value,
      expected = 5
    )
  }

  test("Should work in a graph with a lot of inbounds to one node") {
    assertEquals(
      obtained = Connectivity
        .countDisconnectedSubgraphs(
          SimpleGraph
            .empty[Int]
            .addNode(1)
            .addNode(2)
            .addNode(3)
            .addNode(4)
            .addNode(5)
            .addNode(6)
            .addNode(7)
            .addNode(8)
            .addEdge(2, 1)
            .addEdge(3, 1)
            .addEdge(4, 1)
            .addEdge(5, 1)
            .addEdge(6, 1)
            .addEdge(7, 1)
            .addEdge(8, 1)
        )
        .value,
      expected = 1
    )
  }
}
