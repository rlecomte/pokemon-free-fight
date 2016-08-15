package pkm.models

import scala.util.Random

import pkm.data.Pokedex

case class Dresser(name: String, hp: Int = 500, mana: Int = 0)

case class Team(dresser: Dresser, pokemons: List[Pokemon]) {

  def getAttack() = {
    val cost = pokemons.headOption.map(_.powerAttack.cost).getOrElse(dresser.mana + 1)
    if (cost < dresser.mana) pokemons.head.powerAttack
    else pokemons.head.basicAttack
  }

  def isAlive() = dresser.hp > 0
  def isDead() = !isAlive()
}

object Team {
  private def randomPokemon = Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length))
  def apply(dresser: String): Team = new Team(Dresser(dresser),List.fill(6)(randomPokemon))
}