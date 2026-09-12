package net.andimiller.hedgehogs
package dag.visitor

object Digraph {

  def apply(g: DataGraph[String, Node, Unit]): String =
    s"""
     |digraph G {
     |  bgcolor=transparent;
     |  node [shape=circle style=filled fontname="Arial"];
     |  edge [color=grey];
     |
     |  ${g.nodeMap.toList
      .map { case (n, node) =>
        // hexagons render as polygons, which graph.css covers alongside ellipses
        val shape = node match {
          case Node.Approval => " shape=hexagon"
          case _             => ""
        }
        s"  $n [id=\"$n\" fillcolor=\"var(--$n-fill)\"$shape];"
      }
      .mkString("\n")}
     |
     |  ${g.edges.map { case (from, to) => s"  $from -> $to;" }.mkString("\n")}
     |}
     """.stripMargin

}
