package net.andimiller.hedgehogs
package dag.visitor

object Digraph {

  def apply(g: DataGraph[String, State, Unit]): String =
    s"""
     |digraph G {
     |  bgcolor=transparent;
     |  color=grey;
     |  node [shape=circle color=grey style=filled fontname="Arial"];
     |  edge [color=grey];
     |
     |  ${g.nodeMap.toList.map { case (n, state) => s"  $n [id=\"$n\" fillcolor=${state.toColour}];" }.mkString("\n")}
     |
     |  ${g.edges.map { case (from, to) => s"  $from -> $to;" }.mkString("\n")}
     |}
     """.stripMargin

}
