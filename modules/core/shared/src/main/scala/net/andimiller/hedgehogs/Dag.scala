package net.andimiller.hedgehogs

import cats.Eval

object Dag {

  def validate[Id, Data, Distance](g: Graph[Id, Data, Distance]): Eval[Either[String, Unit]] = {
    // identify nodes without an inbound
    val hasInbound = g.edges.view.mapValues(_.map(_._1)).values.flatten.toSet
    val removeMe = g.nodes.keySet -- hasInbound
    if (removeMe.isEmpty) {
      if (g.nodes.nonEmpty) {
        Eval.now(Left(s"Detected cycles between: ${g.nodes.keys.mkString(",")}"))
      } else {
        Eval.now(Right(()))
      }
    } else {
      Eval.later {
        removeMe.foldLeft(g)(_.removed(_))
      }.flatMap(validate)
    }
  }

  def isDag[Id, Data, Distance](g: Graph[Id, Data, Distance]): Eval[Boolean] =
    validate(g).map(_.isRight)

}
