package net.andimiller.hedgehogs.circe

import cats.effect.IO
import cats.implicits._
import fs2.io.file.Path
import io.circe.{Decoder, Json}
import net.andimiller.hedgehogs._
import net.andimiller.munit.cats.effect.styles.WordIOSpec

class CirceSpec extends WordIOSpec {

  "Circe Module" should {
    "parse a node" in {
      import io.circe.generic.auto._
      case class Data(name: String)
      IO.fromEither(
        Json
          .obj(
            "id"   -> Json.fromLong(1234),
            "data" -> Json.obj("name" -> Json.fromString("test name"))
          )
          .as[Node[BigDecimal, Data]]
      ).assertEquals(
        Node(BigDecimal(1234), Data("test name"))
      )
    }
    "parse an edge" in {
      IO.fromEither(
        Json
          .obj(
            "from"   -> Json.fromLong(1),
            "to"     -> Json.fromLong(2),
            "weight" -> Json.fromBigDecimal(BigDecimal(0.6))
          )
          .as[Edge[Int, BigDecimal]]
      ).assertEquals(
        Edge(1, 2, BigDecimal(0.6))
      )
    }

  }

}
