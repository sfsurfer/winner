package com.danlane.winner.model

import com.danlane.winner.model.CardFace.Three
import com.danlane.winner.model.Suit.Diamond

case class Player(name: String, hand: Option[Hand] = None) {

  val isEmpty = false

  def has3ofDiamonds(): Boolean = hand.exists(_.cards.exists{ card: Card =>
    card.value == Card.getValue(Three, Diamond)
  })

  def playAny(): (Player, PlayableHand) = {
    // TODO: prompt something...
    //  For now, just play low single
    val cards: Vector[Card] = this.hand.map(_.cards.head).toVector
    val play = PlayableHand.playHand(cards) // TODO: use this as a test
    val newHand = for {
      curr <- this.hand
      played <- play
    } yield Hand(curr.cards.diff(played.cards))

    (Player(this.name, newHand), play.get)
  }

  def playLowest(handToBeat: PlayableHand): (Player, PlayableHand) = ???
//  {
//     TODO: IMPLEMENT/FIX THIS...
//    (this, this.hand.getOrElse(Hand(Vector.empty[Card])))
//  }

  override def toString: String =
    s"""
      |Name: $name
      |Hand: ${hand.getOrElse(Hand(Vector.empty[Card]))}
      |""".stripMargin
}

class EmptyPlayer extends Player("", None) { override val isEmpty = true }
