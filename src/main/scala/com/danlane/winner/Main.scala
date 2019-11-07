package com.danlane.winner

import com.danlane.winner.engine.Game
import com.danlane.winner.model.Player

object Main extends App {
  println("Starting Winner!!")
  println("How many players? ")
  // TODO: Needs to be in try/catch and loop until valid
  val playerCount = scala.io.StdIn.readInt()
  println(s"player count = $playerCount")

  val players = (1 to playerCount).map { i =>
    val name = scala.io.StdIn.readLine(s"Enter player $i's name: ")
    Player(name)
  }.toVector

  println("Starting Game!!")
  var result = Game.playGame(players)

  //
}
