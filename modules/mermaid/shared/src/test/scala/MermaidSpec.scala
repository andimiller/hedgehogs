import cats.implicits._
import net.andimiller.hedgehogs.{AdjacencyListDataGraph, DataGraph}
import net.andimiller.hedgehogs.mermaid.Mermaid
import net.andimiller.hedgehogs.mermaid.Mermaid.{MermaidEdge, MermaidShape}

class MermaidSpec extends munit.FunSuite {

  test("Generate a flowchart from a valid DAG") {

    val graph = DataGraph
      .empty[String, String, Int]
      .addNode("A", "Node A")
      .addNode("B", "Node B")
      .addNode("C", "Node C This ❤ Unicode")
      .addNode(
        "D",
        """Node D
             |Multiline""".stripMargin
      )
      .addNode("E", "This **is** _Markdown_")
      .addEdge("A", "B", 2)
      .addEdge("B", "C", 4)
      .addEdge("C", "D", 7)
      .addEdge("C", "E", 4)

    val mermaid = Mermaid.flowchart(graph)(
      extractNodeId = identity,
      extractEdgeName = _.toString.some,
      extractNodeName = _.some,
      extractNodeType = {
        case "Node A" => MermaidShape.Hexagon.some
        case _        => None
      },
      extractEdgeType = {
        case 2 => MermaidEdge.ThickArrow.some
        case _ => None
      }
    )(
      markdown = true
    )

    assertEquals(
      mermaid,
      """flowchart TD
        |  E["`This **is** _Markdown_`"]
        |  A{{"`Node A`"}}
        |  B["`Node B`"]
        |  C["`Node C This ❤ Unicode`"]
        |  D["`Node D
        |Multiline`"]
        |  A ==>|"`2`"| B
        |  B -->|"`4`"| C
        |  C -->|"`7`"| D
        |  C -->|"`4`"| E
        |""".stripMargin
    )

  }

}
