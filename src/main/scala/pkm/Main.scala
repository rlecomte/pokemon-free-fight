package pkm

import scala.util.Random

import pkm.data.Pokedex

object Main extends App {

  println(Pokedex.pokemons.mkString("\n"))

  val randomPokemon = Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length))
  println(s"Random: $randomPokemon")


  val arena = new Arena

  arena.fight(Team(Dresser("Sacha")), Team(Dresser("Regis")))

}
