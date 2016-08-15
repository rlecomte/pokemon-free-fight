package pkm

import pkm.models.{Attack, Pokemon, PowerAttack, Team}

sealed trait Action {

  def team: Team

  def fight(action: Action): Team

  protected def minusMana(t: Team, a: Attack) = a match {
    case a: PowerAttack => t.copy(dresser = t.dresser.copy(mana = t.dresser.mana - a.cost))
    case _ => t
  }

  protected def minusDresserHp(t: Team, a: Attack, percent: Float = 1.0f) = {
    val newHp = (a.damage * percent).toInt
    //println(s"Dresser ${t.dresser.name} is attacked and suffer of $newHp damage !")
    t.copy(dresser = t.dresser.copy(hp = t.dresser.hp - (a.damage * percent).toInt))
  }

  protected def minusPokemonHp(t: Team, a: Attack, percent: Float = 1.0f) = {

    def newPokemons(): List[Pokemon] = {
      if (t.pokemons.isEmpty) t.pokemons
      else {
        val p = t.pokemons.head
        val newHp = p.stats.hp - (a.damage * percent).toInt
        //println(s"Pokemon ${p.name} of ${t.dresser.name} is attacked and suffer of $newHp damage !")
        if (newHp > 0) p.copy(stats = p.stats.copy(hp = newHp)) :: t.pokemons.tail
        else t.pokemons.tail
      }
    }

    t.copy(pokemons = newPokemons())
  }

}

trait IncManaAction extends Action {

  abstract override def fight(action: Action): Team = incMana(super.fight(action))

  private def incMana(t: Team): Team = {
    t.copy(dresser = t.dresser.copy(mana = t.dresser.mana + 1))
  }
}

sealed class DefendDresser(val team: Team) extends Action {
  override def fight(action: Action): Team = action match {
    case AttackDresser(a) => minusDresserHp(team, a, 0.25f)
    case DefendDresser(_) => team
    case AttackPokemon(a) => minusPokemonHp(team, a, 0.75f)
  }
}
object DefendDresser {
  def apply(team: Team): DefendDresser = new DefendDresser(team) with IncManaAction
  def unapply(x: DefendDresser) = Some(x.team)
}

sealed class AttackDresser(_team: Team, attack: Attack) extends Action {

  override def fight(action: Action): Team = action match {
    case AttackDresser(a) => minusDresserHp(team, a)
    case DefendDresser(_) => minusDresserHp(team, attack, 0.50f)
    case AttackPokemon(a) => minusPokemonHp(team, a)
  }

  def team = minusMana(_team, attack)
}
object AttackDresser {
  def apply(team: Team): AttackDresser = new AttackDresser(team, team.getAttack()) with IncManaAction
  def unapply(x: AttackDresser) = Some(x.team.getAttack())
}

sealed class AttackPokemon(_team: Team, attack: Attack) extends Action {

  override def fight(action: Action): Team = action match {
    case AttackDresser(a) => minusDresserHp(team, a)
    case DefendDresser(_) => team
    case AttackPokemon(a) => minusPokemonHp(team, a)
  }

  def team = minusMana(_team, attack)
}
object AttackPokemon {
  def apply(team: Team): AttackPokemon = new AttackPokemon(team, team.getAttack()) with IncManaAction
  def unapply(x: AttackPokemon) = Some(x.team.getAttack())
}

object Fight {
  def apply(action: Action, action2: Action): (Team, Team) = {
    (action.fight(action2), action2.fight(action))
  }
}
