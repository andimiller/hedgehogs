package net.andimiller.hedgehogs.dag.visitor

object GenerateCSS extends App {

  val nodeNames: Seq[String] = (('A' to 'Z') ++ ('a' to 'z')).map(_.toString)

  val vars =
    s""":root {
${nodeNames.map(n => s"  --$n-fill: white;").mkString("\n")}
}
"""

  val css =
    nodeNames
      .map { n =>
        s"g.node#$n ellipse { fill: var(--$n-fill); }"
      }
      .mkString("\n")

  println(vars)
  println(css)

}
