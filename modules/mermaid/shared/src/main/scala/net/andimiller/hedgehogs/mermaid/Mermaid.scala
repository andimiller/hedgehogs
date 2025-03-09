package net.andimiller.hedgehogs.mermaid

import cats.implicits.showInterpolator
import cats.Show
import net.andimiller.hedgehogs.DataGraph

object Mermaid {

  def escapeText(markdown: Boolean)(s: String) =
    if (markdown)
      s""""`$s`""""
    else if (
      s.exists(_.isWhitespace) ||
      s.exists(Character.UnicodeBlock.of(_) != Character.UnicodeBlock.BASIC_LATIN)
    )
      s""""$s""""
    else s

  sealed trait MermaidDirection
  object MermaidDirection {
    case object TopDown   extends MermaidDirection
    case object LeftRight extends MermaidDirection

    implicit val show: Show[MermaidDirection] = {
      case TopDown   => "TD"
      case LeftRight => "LR"
    }
  }

  case class MermaidShapeWithContent(shape: MermaidShape, s: String, markdown: Boolean) {
    lazy val render: String =
      shape.start + escapeText(markdown)(s) + shape.end
  }
  object MermaidShapeWithContent                                                        {
    implicit val show: Show[MermaidShapeWithContent] = _.render
  }

  sealed abstract class MermaidShape(val start: String, val end: String) {
    def withContent(s: String, markdown: Boolean): MermaidShapeWithContent = MermaidShapeWithContent(this, s, markdown)
  }
  object MermaidShape                                                    {
    case object Square           extends MermaidShape("[", "]")
    case object Round            extends MermaidShape("(", ")")
    case object Stadium          extends MermaidShape("([", "])")
    case object Subroutine       extends MermaidShape("[[", "]]")
    case object Cylinder         extends MermaidShape("[(", ")]")
    case object Rhombus          extends MermaidShape("{", "}")
    case object Hexagon          extends MermaidShape("{{", "}}")
    case object Parallelogram    extends MermaidShape("[/", "/]")
    case object ParallelogramAlt extends MermaidShape("[\\", "\\]")
    case object Trapezoid        extends MermaidShape("[/", "\\]")
    case object TrapezoidAlt     extends MermaidShape("[\\", "/]")
    case object DoubleCircle     extends MermaidShape("(((", ")))")
  }

  case class MermaidEdgeWithContent(edge: MermaidEdge, content: String, markdown: Boolean) {
    lazy val render: String =
      edge.render + Option(content).filter(_.nonEmpty).map(escapeText(markdown)).map { s => s"|$s|" }.getOrElse("")
  }
  object MermaidEdgeWithContent                                                            {
    implicit val show: Show[MermaidEdgeWithContent] = _.render
  }

  sealed class MermaidEdge(val one: String, val two: String, val three: String, chosen: Int = 1) {
    private val validLengths = Set(1, 2, 3)

    def withContent(s: String, markdown: Boolean): MermaidEdgeWithContent = MermaidEdgeWithContent(this, s, markdown)

    def render: String             = chosen match {
      case 2 => two
      case 3 => three
      case _ => one
    }
    def apply(i: Int): MermaidEdge = if (validLengths.contains(i)) new MermaidEdge(one, two, three, chosen) else this
  }

  object MermaidEdge {
    case object Link               extends MermaidEdge("---", "----", "-----")
    case object Arrow              extends MermaidEdge("-->", "--->", "---->")
    case object Thick              extends MermaidEdge("===", "====", "=====")
    case object ThickArrow         extends MermaidEdge("==>", "===>", "====>")
    case object Invisible          extends MermaidEdge("~~~", "~~~~", "~~~~~")
    case object Dotted             extends MermaidEdge("-.-", "-..-", "-...-")
    case object DottedArrow        extends MermaidEdge("-.->", "-..->", "-...->")
    case object BidirectionalArrow extends MermaidEdge("<-->", "<--->", "<---->")

    implicit val show: Show[MermaidEdge] = (t: MermaidEdge) => t.render
  }

  def flowchart[Id, NodeData, EdgeData](g: DataGraph[Id, NodeData, EdgeData])(
      extractNodeId: Id => String, // we need this one to make the graph make sense
      extractNodeName: NodeData => Option[String] = (_: NodeData) => None,
      extractNodeType: NodeData => Option[MermaidShape] = (_: NodeData) => None,
      extractEdgeName: EdgeData => Option[String] = (_: EdgeData) => None,
      extractEdgeType: EdgeData => Option[MermaidEdge] = (_: EdgeData) => None
  )(
      direction: MermaidDirection = MermaidDirection.TopDown,
      defaultNode: MermaidShape = MermaidShape.Square,
      defaultEdge: MermaidEdge = MermaidEdge.Arrow,
      markdown: Boolean = false
  ): String = {
    // assuming it's a dag, add a check later
    def formatName(s: String) = // add square node type by default
      if (s.head.isLetterOrDigit) "[" + s + "]" else s

    val nodes = g.nodeMap.toVector
      .map { case (nodeId, nodeData) =>
        val id       = extractNodeId(nodeId)
        val name     = extractNodeName(nodeData).getOrElse(id)
        val nodeType = extractNodeType(nodeData).getOrElse(defaultNode)

        show"  $id${nodeType.withContent(name, markdown)}"
      }
      .mkString(System.lineSeparator())

    val edges = g.edgeMap.toVector
      .map { case ((from, to), edgeData) =>
        val fromId   = extractNodeId(from)
        val toId     = extractNodeId(to)
        val edgeName = extractEdgeName(edgeData).getOrElse("")
        val edgeType = extractEdgeType(edgeData).getOrElse(defaultEdge)

        show"  $fromId ${edgeType.withContent(edgeName, markdown)} $toId"
      }
      .mkString(System.lineSeparator())

    show"""flowchart $direction
          |$nodes
          |$edges
          |""".stripMargin
  }

}
