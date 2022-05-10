package net.andimiller.hedgehogs

import cats.data.NonEmptyList
import cats.implicits._

class GraphSpec extends munit.FunSuite {
  test("fail if a node is defined twice") {
    assertEquals(
      Graph.fromIterables(
        Vector(Node("abc", "cool"), Node("abc", "not cool")),
        Vector.empty,
        bidirectional = false
      ),
      "abc has 2 nodes, required at most 1".invalidNel
    )
  }
  test("fail if either end of an edge doesn't exist") {
    assertEquals(
      Graph.fromIterables(
        Vector(Node("abc", "cool")),
        Vector(Edge("abc", "def", 10), Edge("ghi", "abc", 5)),
        bidirectional = false
      ),
      NonEmptyList.of("def is not a known node", "ghi is not a known node").invalid
    )
  }
  test("load a good graph") {
    assertEquals(
      Graph.fromIterables(
        Vector(Node("abc", "cool"), Node("def", "also cool")),
        Vector(Edge("abc", "def", 10)),
        bidirectional = false
      ),
      Graph(
        Map("abc" -> "cool", "def" -> "also cool"),
        Map("abc" -> Vector(("def", 10)))
      ).validNel
    )
  }
  test("load a good graph with bidirectional") {
    assertEquals(
      Graph.fromIterables(
        Vector(Node("abc", "cool"), Node("def", "also cool")),
        Vector(Edge("abc", "def", 10)),
        bidirectional = true
      ),
      Graph(
        Map("abc" -> "cool", "def"              -> "also cool"),
        Map("abc" -> Vector(("def", 10)), "def" -> Vector(("abc", 10)))
      ).validNel
    )
  }
}
