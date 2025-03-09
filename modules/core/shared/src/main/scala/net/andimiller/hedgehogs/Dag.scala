package net.andimiller.hedgehogs

import cats.Eval

object Dag {

  def validate[Id](g: SimpleGraph[Id]): Eval[Either[String, Unit]] = {
    // identify nodes without an inbound
    val hasInbound = g.nodes.filter { id => g.inbound(id).nonEmpty }
    val removeMe   = g.nodes -- hasInbound
    if (removeMe.isEmpty) {
      if (g.nodes.nonEmpty) {
        Eval.now(Left(s"Detected cycles between: ${g.nodes.mkString(",")}"))
      } else {
        Eval.now(Right(()))
      }
    } else {
      Eval
        .later {
          removeMe.foldLeft(g)(_.removeNode(_))
        }
        .flatMap(validate)
    }
  }

  def isDag[Id](g: SimpleGraph[Id]): Eval[Boolean] =
    validate(g).map(_.isRight)

}
