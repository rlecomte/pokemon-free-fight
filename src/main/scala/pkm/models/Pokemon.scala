package pkm.models

import io.circe.Decoder

case class Pokemon(
  number: Int,
  name: String,
  `type`: String,
  stats: PokemonStats,
  basicAttack: BasicAttack,
  powerAttack: PowerAttack
) {
  override def toString = s"""Pokemon $number "$name" (${`type`}) - HP: ${stats.hp}"""
}

case class PokemonType(name: String) extends AnyVal
case class PokemonStats(hp: Int, speed: Int)

trait Attack {
  val damage: Int
}
object NullAttack extends Attack{
  val damage = 0
}
case class BasicAttack(name: String, `type`: String, damage: Int) extends Attack
case class PowerAttack(name: String, `type`: String, damage: Int, cost: Int) extends Attack

trait PokemonDecoder {
  import io.circe.generic.semiauto._

  implicit protected val basicAttackDecoder = deriveDecoder[BasicAttack]
  implicit protected val powerAttackDecoder = deriveDecoder[PowerAttack]
  implicit protected val statsDecoder = deriveDecoder[PokemonStats]
  implicit protected val pokemonDecoder = Decoder.forProduct6("number", "name", "type", "stats", "basic-attack", "power-attack")(Pokemon.apply)
}
