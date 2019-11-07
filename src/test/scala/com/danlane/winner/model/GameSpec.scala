package com.danlane.winner.model

import com.danlane.winner.engine.Game
import com.danlane.winner.engine.Game.GameResult
import org.scalatest.{FlatSpec, Matchers}

class GameSpec extends FlatSpec with Matchers {
//  import CardFace._
//  import Suit._

  val players = Vector(
    Player("a"),
    Player("b"),
    Player("c"),
    Player("d")
  )

  "init" should "deal cards to players" in {
    val playersWithHands = Game.init(players)
    playersWithHands.map { p =>
      p.hand.map { h => h.cards.length should be (13) }
    }
  }

  "initialize hand" should "set player with 3 of diamonds first when no game results, preserving order" in {
    val playersWithHands = Game.init(players)
    val with3OfDiamonds = playersWithHands.filter { p => p.has3ofDiamonds() }
    val reordered = Game.initializeHand(playersWithHands, None)
    reordered.head should be (with3OfDiamonds.head)
  }

  it should "set losers to go first" in {
    val results = GameResult(Some(players(0)), Some(players(2)))
    val afterDeal = Game.init(players)

    val afterSwap = Game.initializeHand(afterDeal, Some(results))
    afterSwap.head.name should be (players(2).name)
  }

  // TODO: Need to update this for a selected card
  it should "swap losers highest and winners lowest" in {
    val results = GameResult(Some(players(0)), Some(players(2)))
    val afterDeal = Game.init(players)
    val losersHighest: Card = afterDeal(2).hand.flatMap(_.highestCard).get
    val winnersLowest: Card = afterDeal(0).hand.flatMap(_.lowestCard).get

    val afterSwap = Game.initializeHand(afterDeal, Some(results))

    afterSwap.head.name should be (players(2).name)
    afterSwap.head.hand.exists(_.cards.contains(winnersLowest)) should be (true)
    afterSwap(2).hand.exists(_.cards.contains(losersHighest)) should be (true)
  }
}
