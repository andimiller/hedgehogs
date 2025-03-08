package net.andimiller.hedgehogs

import cats.implicits.catsSyntaxEitherId

class DagSpec extends munit.FunSuite {
  test("Validate a dag") {
    val g = Graph.fromIterables(
      nodes = Vector(Node("a", ()), Node("b", ()), Node("c", ())),
      edges = Vector(Edge("a", "b", 1), Edge("b", "c", 1)),
      bidirectional = false
    ).toOption.get
    assertEquals(
      Dag.validate(g).value,
      ().asRight
    )
  }

  test("Validate a dag with a loop") {
    val g = Graph.fromIterables(
      nodes = Vector(Node("a", ()), Node("b", ()), Node("c", ())),
      edges = Vector(Edge("a", "b", 1), Edge("b", "c", 1), Edge("c", "a", 1)),
      bidirectional = false
    ).toOption.get
    assertEquals(
      Dag.validate(g).value,
      "Detected cycles between: a,b,c".asLeft
    )
  }
}
