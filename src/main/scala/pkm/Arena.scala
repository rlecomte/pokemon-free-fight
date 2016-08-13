package pkm

import scala.util.Random

import pkm.data.Pokedex
import pkm.models._

class Arena {

  def fight(team1: Team, team2: Team) = {

    println("First team : " )
    println(team1)
    println("Second team : ")
    println(team2)
    println("")

    val player1 = RandomDresserIA
    val player2 = RandomDresserIA

    def recursive(team1: Team, team2: Team): Unit = {
      println("------------------------------------------------")
      println("")
      if(team1.dresser.hp <= 0) println(s"${team2.dresser.name} win!!!")
      else if(team2.dresser.hp <= 0) println(s"${team1.dresser.name} win!!!")
      else {
        val action1 = player1.action(team1, team2)
        val action2 = player2.action(team2, team1)
        recursive(action1.fight(action2), action2.fight(action1))
      }
    }

    recursive(team1, team2)

  }

}

sealed trait Action {

  def team: Team

  def fight(action: Action): Team = team

  protected def minusMana(t: Team, a: Attack) = a match {
    case a: PowerAttack => t.copy (dresser = t.dresser.copy (mana = t.dresser.mana - a.cost) )
    case _ => t
  }

  protected def minusDresserHp(t: Team, a: Attack, percent: Float = 1.0f) = {
    val newHp = (a.damage * percent).toInt
    println(s"Dresser ${t.dresser.name} is attacked and suffer of $newHp damage !")
    t.copy(dresser = t.dresser.copy(hp = t.dresser.hp - (a.damage * percent).toInt))
  }

  protected def minusPokemonHp(t: Team, a: Attack, percent: Float = 1.0f) = {

    def newPokemons(): List[Pokemon] ={
      if(t.pokemons.isEmpty) t.pokemons
      else {
        val p = t.pokemons.head
        val newHp = p.stats.hp - (a.damage * percent).toInt
        println(s"Pokemon ${p.name} of ${t.dresser.name} is attacked and suffer of $newHp damage !")
        if(newHp > 0) p.copy(stats = p.stats.copy(hp = newHp)) :: t.pokemons.tail
        else t.pokemons.tail
      }
    }

    t.copy(pokemons = newPokemons())
  }

}

trait IncManaAction extends Action {

  abstract override def fight(action: Action): Team = incMana(super.fight(action))

  private def incMana(t: Team): Team = t.copy(dresser = t.dresser.copy(mana = t.dresser.mana + 1))
}

abstract class BaseAction extends Action with IncManaAction

case class DefendDresser(team: Team) extends BaseAction {

  override def fight(action: Action): Team = action match {
      case AttackDresser(_, a) => minusDresserHp(team, a, 0.25f)
      case DefendDresser(_) => team
      case AttackPokemon(_, a) =>  minusPokemonHp(team, a, 0.75f)
  }

}

case class AttackDresser(_team: Team, attack: Attack) extends BaseAction {

  override def fight(action: Action): Team = action match {
      case AttackDresser(_, a) => minusDresserHp(team, a)
      case DefendDresser(_) => minusDresserHp(team, attack, 0.50f)
      case AttackPokemon(_, a) =>  minusPokemonHp(team, a)
  }

  def team = minusMana(_team, attack)
}

case class AttackPokemon(_team: Team, attack: Attack) extends BaseAction {

  override def fight(action: Action): Team = action match {
    case AttackDresser(_, a) => minusDresserHp(team, a)
    case DefendDresser(_) => team
    case AttackPokemon(_, a) => minusPokemonHp(team, a)
  }

  def team = minusMana(_team, attack)
}

sealed trait IA {
  def action(team: Team, opponent: Team): Action
}
object RandomDresserIA extends IA {
  override def action(team: Team, opponent: Team) = {

    def getAttack() = {
      val cost = team.pokemons.headOption.map(_.powerAttack.cost).getOrElse(team.dresser.mana + 1)
      if (cost < team.dresser.mana) team.pokemons.head.powerAttack
      else team.pokemons.head.basicAttack
    }

    val random = Random.nextInt(3)
    if(random == 0) AttackDresser(team, getAttack())
    else if(random == 1) DefendDresser(team)
    else AttackPokemon(team, getAttack())
  }
}

case class Dresser(name: String, hp: Int = 500, mana: Int = 0)

case class Team(dresser: Dresser, pokemons: List[Pokemon])

object Team {
  private def randomPokemon = Pokedex.pokemons(Random.nextInt(Pokedex.pokemons.length))
  def apply(dresser: Dresser): Team = new Team(dresser,List.fill(6)(randomPokemon))
}
