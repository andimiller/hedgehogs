package net.andimiller.hedgehogs

import cats.Eval

import scala.annotation.tailrec
import scala.util.Random

object Connectivity {

  def countDisconnectedSubgraphs[Id](g: SimpleGraph[Id]): Eval[Int] = {

    def recurse(visited: Set[Id], otherComponents: Int): Eval[Int] = {
      Eval
        .later {
          visited.flatMap(g.outgoing) ++ visited.flatMap(g.inbound) -- visited
        }
        .flatMap { newNodes =>
          if (newNodes.isEmpty) {
            // we finished building this component
            val remaining = g.nodes -- visited
            remaining.headOption match {
              case None                   =>
                Eval.now(otherComponents + 1)
              case Some(nextStartingNode) =>
                recurse(visited + nextStartingNode, otherComponents + 1)
            }
          } else {
            val expanded = visited ++ newNodes
            recurse(expanded, otherComponents)
          }
        }
    }

    g.nodes.headOption match {
      case Some(chosenNode) => recurse(Set(chosenNode), 0)
      case None             => Eval.now(0)
    }
  }

}
