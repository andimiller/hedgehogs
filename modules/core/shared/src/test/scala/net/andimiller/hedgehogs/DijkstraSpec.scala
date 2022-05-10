package net.andimiller.hedgehogs

import cats.implicits._

class DijkstraSpec extends munit.FunSuite {
  test("route in a directional graph") {
    assertEquals(
      Graph
        .fromIterables(
          Vector(Node("abc", "cool"), Node("def", "also cool")),
          Vector(Edge("abc", "def", 10)),
          bidirectional = false
        )
        .map { g =>
          val r1 = Dijkstra(g)("abc", "def")
          val r2 = Dijkstra(g)("def", "abc")
          (r1, r2)
        },
      (Some((10, List("abc", "def"))), None).validNel
    )
  }
  test("route in a bidirectional graph") {
    assertEquals(
      Graph
        .fromIterables(
          Vector(Node("abc", "cool"), Node("def", "also cool")),
          Vector(Edge("abc", "def", 10)),
          bidirectional = true
        )
        .map { g =>
          val r1 = Dijkstra(g)("abc", "def")
          val r2 = Dijkstra(g)("def", "abc")
          (r1, r2)
        },
      (Some((10, List("abc", "def"))), Some((10, List("def", "abc")))).validNel
    )
  }
  test("multi-route in a directional graph") {
    assertEquals(
      Graph
        .fromIterables(
          Vector(Node("abc", "cool"), Node("def", "also cool")),
          Vector(Edge("abc", "def", 10)),
          bidirectional = false
        )
        .map { g =>
          val r1 = Dijkstra.multi(g)("abc", Set("abc", "def"))
          val r2 = Dijkstra.multi(g)("def", Set("abc", "def"))
          (r1, r2)
        },
      (
        Map(
          "abc" -> (0, List("abc")),
          "def" -> (10, List("abc", "def"))
        ),
        Map(
          "def" -> (0, List("def"))
        )
      ).validNel
    )
  }
}
