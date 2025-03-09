package net.andimiller.hedgehogs

import cats.implicits._

class DijkstraSpec extends munit.FunSuite {
  test("route in a directional graph") {
    val g = DataGraph
      .empty[String, String, Int]
      .addNode("abc")
      .addNode("def")
      .addEdge("abc", "def", 10)
    assertEquals(
      obtained = Dijkstra(g)("abc", "def"),
      expected = Some((10, List("abc", "def")))
    )
    assertEquals(
      obtained = Dijkstra(g)("def", "abc"),
      expected = None
    )
  }
  test("find the shortest route in a graph") {
    val g = new AdjacencyListDataGraph[Int, String, Double](
      nodeMap = Map(
        (1 -> "one"),
        (2 -> "two"),
        (3 -> "three"),
        (4 -> "four"),
        (5 -> "five")
      ),
      edgeMap = Map(
        (1 -> 2) -> 1.0,
        (2 -> 3) -> 2.0,
        (3 -> 4) -> 3.0,
        (4 -> 5) -> 4.0,
        (1 -> 5) -> 15.0, // expensive single hop,
        (1 -> 3) -> 2.0   // slightly cheaper way to get to 3
      )
    )
    assertEquals(
      obtained = Dijkstra(g)(from = 1, to = 5),
      expected = Some(9.0, List(1, 3, 4, 5))
    )
  }
  test("route in a bidirectional graph") {
    val g = DataGraph
      .empty[String, String, Int]
      .addNode("abc")
      .addNode("def")
      .addEdge("abc", "def", 10)
      .addEdge("def", "abc", 5)
    assertEquals(
      obtained = Dijkstra(g)("abc", "def"),
      expected = Some((10, List("abc", "def")))
    )
    assertEquals(
      obtained = Dijkstra(g)("def", "abc"),
      expected = Some((5, List("def", "abc")))
    )
  }
  test("multi-route in a directional graph") {
    val g =
      DataGraph
        .empty[String, String, Int]
        .addNode("abc")
        .addNode("def")
        .addEdge("abc", "def", 10)

    assertEquals(
      obtained = Dijkstra.multi(g)("abc", Set("abc", "def")),
      expected = Map(
        "abc" -> (0, List("abc")),
        "def" -> (10, List("abc", "def"))
      )
    )
  }
}
