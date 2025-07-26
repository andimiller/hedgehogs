package net.andimiller.hedgehogs
package dag.visitor

object Digraph {

  def distanceColour(d: Option[Int]): String =
    d match {
      case None => "indianred"
      case Some(0) => "blue"
      case Some(_) => "limegreen"
    }


  def apply(currentNode: String, g: DataGraph[String, Option[Int], Unit]): String =
    s"""
     |digraph G {
     |  bgcolor=transparent;
     |  node [shape=circle style=filled fontname="Arial"];
     |  edge [color=grey];
     |
     |  ${g.nodeMap.toList.map { case (n, distance) => s"  $n [id=\"$n\" label=\"${distance.fold(n)(_.toString)}\" fillcolor=\"${if (n == currentNode) "turquoise" else distanceColour(distance)}\"] style=\"filled\";" }.mkString("\n")}
     |
     |  ${g.edges.map { case (from, to) => s"  $from -> $to;" }.mkString("\n")}
     |}
     """.stripMargin

}
