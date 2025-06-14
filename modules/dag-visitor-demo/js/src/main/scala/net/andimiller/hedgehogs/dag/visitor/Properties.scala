package net.andimiller.hedgehogs.dag.visitor

import tyrian.Cmd
import org.scalajs.dom.document
import scalajs.js
import cats.effect.Sync

@js.native
trait StyledElement extends js.Object {
  def style: Style = js.native
}

@js.native
trait Style extends js.Object {
  def setProperty(name: String, value: String): Unit = js.native
}

object CssVariables {
  def set[F[_]: Sync, Msg](name: String, value: String): Cmd[F, Msg] =
    Cmd.SideEffect {
      val root: StyledElement = document.documentElement.asInstanceOf[StyledElement]
      root.style.setProperty(name, value)
    }
}
