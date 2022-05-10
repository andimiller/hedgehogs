package net.andimiller.hedgehogs

import io.circe.{Encoder, Decoder}
import io.circe.generic.semiauto._

package object circe {
  implicit def nodeEncoder[Id: Encoder, Data: Encoder]: Encoder[Node[Id, Data]] = deriveEncoder
  implicit def nodeDecoder[Id: Decoder, Data: Decoder]: Decoder[Node[Id, Data]] = deriveDecoder

  implicit def edgeEncoder[Id: Encoder, Distance: Encoder]: Encoder[Edge[Id, Distance]] = deriveEncoder
  implicit def edgeDecoder[Id: Decoder, Distance: Decoder]: Decoder[Edge[Id, Distance]] = deriveDecoder
}
