package com.danlane.winner.model

import com.danlane.winner.model.CardFace._
import com.danlane.winner.model.Suit.Diamond

case class Player(name: String, hand: Option[Hand] = Option.empty[Hand]) {

  val hasNoCards: Boolean = hand.isEmpty || hand.exists(_.cards.isEmpty)

  val hasCards: Boolean = !hasNoCards

  def has3ofDiamonds(): Boolean = hand.exists(_.cards.exists{ card: Card =>
    card.value == Card.getValue(Three, Diamond)
  })

  override def toString: String =
    s"""
      |Name: $name
      |Hand: ${hand.getOrElse(Hand(Vector.empty[Card]))}
      |""".stripMargin

  def ===(that: Player): Boolean = this.name == that.name
}

object Player {
  def apply(name: String): Player = new Player(name)
}

