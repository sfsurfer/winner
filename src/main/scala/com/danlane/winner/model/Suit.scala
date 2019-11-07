package com.danlane.winner.model

sealed trait Suit {
  def value: Int
}

object Suit {

  object Spade extends Suit {
    val value = 3
    override def toString = "Spade"
  }

  object Heart extends Suit {
    val value = 2
    override def toString = "Heart"
  }

  object Club extends Suit {
    val value = 1
    override def toString = "Club"
  }

  object Diamond extends Suit {
    val value = 0
    override def toString = "Diamond"
  }

  def allSuits: Vector[Suit] = Vector(Diamond, Club, Heart, Spade)

  def incrSuit(suit: Suit): Suit = {
    val next = if (suit == Spade) { 0 } else { allSuits.indexOf(suit) + 1 }
    allSuits(next)
  }
}
