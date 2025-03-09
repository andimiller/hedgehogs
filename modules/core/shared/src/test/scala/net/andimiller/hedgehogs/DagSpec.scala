package net.andimiller.hedgehogs

import cats.implicits.catsSyntaxEitherId

class DagSpec extends munit.FunSuite {
  test("Validate a dag") {
    val g = SimpleGraph
      .empty[String]
      .addNode("a")
      .addNode("b")
      .addNode("c")
      .addEdge("a", "b")
      .addEdge("b", "c")
    assertEquals(
      Dag.validate(g).value,
      ().asRight
    )
  }

  test("Validate a dag with a loop") {
    val g =
      SimpleGraph
        .empty[String]
        .addNode("a")
        .addNode("b")
        .addNode("c")
        .addEdge("a", "b")
        .addEdge("b", "c")
        .addEdge("c", "a")
    assertEquals(
      Dag.validate(g).value,
      "Detected cycles between: a,b,c".asLeft
    )
  }
}
