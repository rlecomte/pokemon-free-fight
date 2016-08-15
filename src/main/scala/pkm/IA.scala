package pkm

import scala.util.Random

import pkm.models.Team

sealed abstract class IA {
  def action(team: Team, opponent: Team): Action
}

trait LoggerIA extends IA {
  abstract override def action(team: Team, opponent: Team): Action = {
    val a = super.action(team,opponent)
    println(s"${team.dresser.name} choose action ${a.getClass}")
    a
  }
}

class RandomIA extends IA {
  override def action(team: Team, opponent: Team): Action = {
    val random = Random.nextInt(3)
    if (random == 0) AttackDresser(team)
    else if (random == 1) DefendDresser(team)
    else AttackPokemon(team)
  }
}
object RandomIA extends RandomIA with LoggerIA

class MinMaxIA extends IA {

  object State extends Enumeration {
    val attackDresser, attackPokemon, defendDresser = Value

    def toAction(value: Value, team: Team) = value match {
      case State.attackDresser => AttackDresser(team)
      case State.attackPokemon => AttackPokemon(team)
      case State.defendDresser => DefendDresser(team)
    }
  }

  import State._

  def generateAllCombination() = {

    val states = List(attackDresser, attackPokemon, defendDresser)
    val depth = 3

    def rec(x: Int): List[List[State.Value]] = {
      if (x > 0) {
        val r = rec(x - 1)
        for {
          s <- states
          ss <- r
        } yield s :: ss
      } else {
        List(states)
      }
    }

    rec(depth)
  }

  override def action(team: Team, opponent: Team): Action = {
    val action = generateAllCombination().foldLeft(attackDresser) { (selectAction, e) =>
      val (t, o) = e.foldLeft((team, opponent)) { case ((t1, t2), a) =>
        if (t1.isDead() || t2.isDead()) (t1, t2)
        else Fight(State.toAction(a, t1), AttackDresser(t2))
      }
      if (o.isDead()) e.head
      else selectAction
    }

    State.toAction(action, team)
  }
}
object MinMaxIA extends MinMaxIA with LoggerIA

class AggressiveIA extends IA {
  override def action(team: Team, opponent: Team): Action = AttackDresser(team)
}
object AggressiveIA extends AggressiveIA with LoggerIA