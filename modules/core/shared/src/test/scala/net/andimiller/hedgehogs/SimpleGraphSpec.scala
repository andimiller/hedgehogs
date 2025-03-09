package net.andimiller.hedgehogs

class SimpleGraphSpec extends munit.FunSuite {

  test("Build a graph from an empty graph") {
    assertEquals(
      obtained = SimpleGraph
        .empty[Int]
        .addNode(1)
        .addNode(2)
        .addNode(3)
        .addEdge(1, 2)
        .addEdge(2, 3),
      expected = new AdjacencyListSimpleGraph[Int](
        nodes = Set(1, 2, 3),
        edges = Set((1 -> 2), (2 -> 3))
      )
    )
  }

  test("Find neighbours") {
    assertEquals(
      obtained = SimpleGraph
        .empty[Int]
        .addNode(1)
        .addNode(2)
        .addNode(3)
        .addEdge(1, 2)
        .addEdge(2, 3)
        .outgoing(2),
      expected = Set(3)
    )
  }

  test("Removing a node should remove associated edges") {
    assertEquals(
      obtained = SimpleGraph
        .empty[Int]
        .addNode(1)
        .addNode(2)
        .addNode(3)
        .addEdge(1, 2)
        .addEdge(2, 3)
        .removeNode(2),
      expected = SimpleGraph
        .empty[Int]
        .addNode(1)
        .addNode(3)
    )
  }

}
