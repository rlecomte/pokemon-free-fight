package pkm.data

import scala.io.Source

import cats.data.Xor
import pkm.models.{Pokemon, PokemonDecoder}

object Pokedex extends PokemonDecoder {

  val pokemons = {
    val source = Source.fromURL(getClass.getClassLoader.getResource("pokemons.json")).mkString
    io.circe.jawn.decode[Seq[Pokemon]](source) match {
      case Xor.Right(pkms) => pkms
      case Xor.Left(errors) => throw new RuntimeException(s"Cannot load pokemons: $errors")
    }
  }
}
