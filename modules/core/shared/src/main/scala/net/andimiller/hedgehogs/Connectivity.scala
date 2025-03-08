package net.andimiller.hedgehogs

import cats.Eval

import scala.annotation.tailrec
import scala.util.Random

object Connectivity {

  def isConnected[Id, Data, Distance](g: Graph[Id, Data, Distance])(implicit r: Random): Eval[Boolean] = {

    def recurse(visited: Set[Id]): Eval[Boolean] = {
      Eval.later {
        g.edges.toSet.flatMap {
          case (from, to) if visited(from) =>
            to.map(_._1).toSet
          case _ =>
            Set.empty
        }
      }.flatMap { newNodes =>
        if (newNodes.isEmpty) {
          Eval.now(
            visited.size == g.nodes.size
          )
        } else {
          val expanded = visited ++ newNodes
          recurse(expanded)
        }
      }
    }

    val chosenNode = g.nodes.keys.toVector(r.between(0, g.nodes.size))
    recurse(Set(chosenNode))
  }


}
