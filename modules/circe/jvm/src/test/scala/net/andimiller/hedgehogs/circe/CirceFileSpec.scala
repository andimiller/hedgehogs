package net.andimiller.hedgehogs.circe

import cats.effect.IO
import cats.implicits._
import fs2.io.file.Path
import io.circe.Decoder
import net.andimiller.hedgehogs._
import net.andimiller.munit.cats.effect.styles.WordIOSpec

class CirceFileSpec extends WordIOSpec {

  def loadJsonByLine[T: Decoder](s: String): IO[Vector[T]] = fs2.io.file
    .Files[IO]
    .readAll(Path(s))
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .filter(_.nonEmpty)
    .evalMap { s =>
      IO.fromEither(io.circe.parser.parse(s))
    }
    .evalMap { j =>
      IO.fromEither(Decoder[T].decodeJson(j))
    }
    .compile
    .toVector

  "Circe Module" should {
    "load EVE Online systems as nodes" in {
      loadJsonByLine[Node[Long, String]]("./examples/systems.json")
        .map(_.size)
        .assertEquals(8485)
    }
    "load EVE Online gates as Edges" in {
      loadJsonByLine[Edge[Long, Int]]("./examples/gates.json")
        .map(_.size)
        .assertEquals(13764)
    }
    "load the whole EVE database and plan a route" in {
      for {
        systems <- loadJsonByLine[Node[Long, String]]("./examples/systems.json")
        gates   <- loadJsonByLine[Edge[Long, Int]]("./examples/gates.json")
        graph    = Graph.fromIterables(systems, gates, false).toOption.get
        karan    = systems.find(_.data == "Karan").map(_.id).get
        jita     = systems.find(_.data == "Jita").map(_.id).get
        distance = Dijkstra(graph)(karan, jita).map(_._1)
        _       <- IO { assertEquals(distance, 34.some) }
      } yield ()
    }
  }

}
