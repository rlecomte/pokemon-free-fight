package pkm

import scala.util.Random

import pkm.data.Pokedex
import pkm.models.{Dresser, Team}

object Main extends App {

  println(Pokedex.pokemons.mkString("\n"))

  val randomPokemon = Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length))
  println(s"Random: $randomPokemon")


  Arena.fight(Team("Sacha"), Team("Regis"))
}
