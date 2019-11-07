package com.danlane.winner.model

import org.scalatest.{FlatSpec, Matchers}

class PlayerSpec extends FlatSpec with Matchers {

  import CardFace._
  import Suit._

  "has3OfDiamond" should "return true when player has 3 of diamonds" in {
    val hand = Hand(Vector(Card(Three, Diamond)))
    val player = Player("a", Some(hand))
    player.has3ofDiamonds() should be (true)
  }

  it should "return when player does not have 3 of diamonds, but other 3" in {
    val hand = Hand(Vector(Card(Three, Heart)))
    val player = Player("a", Some(hand))
    player.has3ofDiamonds() should be (false)
  }
  it should "return when player does not have 3 of diamonds, but other diamond" in {
    val hand = Hand(Vector(Card(Two, Diamond)))
    val player = Player("a", Some(hand))
    player.has3ofDiamonds() should be (false)
  }
  it should "return when player has empty hand" in {
    val hand = Hand(Vector.empty[Card])
    val player = Player("a", Some(hand))
    player.has3ofDiamonds() should be (false)
  }
  it should "return when player has no hand" in {
    val player = Player("a", None)
    player.has3ofDiamonds() should be (false)
  }
}
