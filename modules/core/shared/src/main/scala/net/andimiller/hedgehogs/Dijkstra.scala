package net.andimiller.hedgehogs

import scala.annotation.tailrec

object Dijkstra {
  def apply[Id, Data, Distance: Numeric](g: Graph[Id, Data, Distance])(from: Id, to: Id): Option[(Distance, List[Id])] =
    run[Id, Data, Distance](g, List((Numeric[Distance].zero, List(from))), to, Set.empty)

  @tailrec
  def run[Id, Data, Distance: Numeric](
      g: Graph[Id, Data, Distance],
      paths: List[(Distance, List[Id])],
      target: Id,
      visited: Set[Id]
  )(implicit ord: Ordering[Distance]): Option[(Distance, List[Id])] = paths match {
    case (distance, path) :: otherPaths =>
      path match {
        case head :: _ if head == target => Some((distance, path.reverse))
        case head :: _                   =>
          val extraPaths: List[(Distance, List[Id])] = g.neighbours(head).toList.flatMap {
            case (key, _) if visited.contains(key) => List.empty
            case (key, d)                          => List((Numeric[Distance].plus(distance, d), key :: path))
          }
          val sortedPaths                            = (extraPaths ++ otherPaths).sortBy(_._1)(ord)
          run(g, sortedPaths, target, visited + head)
        case _                           => None
      }
    case Nil                            => None
  }
}
