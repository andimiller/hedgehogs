package net.andimiller.hedgehogs
package dag.visitor

object Digraph {

  def apply(g: DataGraph[String, Unit, Unit]): String =
    s"""
     |digraph G {
     |  bgcolor=transparent;
     |  node [shape=circle style=filled fontname="Arial"];
     |  edge [color=grey];
     |
     |  ${g.nodeMap.toList.map { case (n, state) => s"  $n [id=\"$n\" fillcolor=\"var(--$n-fill)\"];" }.mkString("\n")}
     |
     |  ${g.edges.map { case (from, to) => s"  $from -> $to;" }.mkString("\n")}
     |}
     """.stripMargin

}
