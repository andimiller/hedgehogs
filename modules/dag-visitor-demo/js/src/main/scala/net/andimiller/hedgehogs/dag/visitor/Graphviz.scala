package net.andimiller.hedgehogs.dag.visitor

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import cats.effect.IO

object GraphvizIO {
  inline def load(): IO[Graphviz] = IO.fromPromise(IO { Graphviz.load() })
}

@js.native
@JSImport("@hpcc-js/wasm-graphviz", "Graphviz")
object Graphviz extends js.Object {
  def load(): js.Promise[Graphviz] = js.native
}

@js.native
trait Graphviz extends js.Object {
  def dot(dotSource: String, outputFormat: String): String = js.native
}
