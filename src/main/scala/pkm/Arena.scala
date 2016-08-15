package pkm

import pkm.models._

object Arena {

  def fight(team1: Team, team2: Team) = {

    println("First team : ")
    println(team1)
    println("Second team : ")
    println(team2)
    println("")

    val player1 = MinMaxIA
    val player2 = AggressiveIA

    def recursive(team1: Team, team2: Team): Unit = {
      println("------------------------------------------------")
      println(s"${team1.dresser.name} [${team1.dresser.hp}]")
      println(s"${team2.dresser.name} [${team2.dresser.hp}]")

      if (team1.isDead()) println(s"${team2.dresser.name} win!!!")
      else if (team2.isDead()) println(s"${team1.dresser.name} win!!!")
      else {
        val action1 = player1.action(team1, team2)
        val action2 = player2.action(team2, team1)
        val (t1, t2) = Fight(action1, action2)
        recursive(t1, t2)
      }
    }

    recursive(team1, team2)

  }

}


