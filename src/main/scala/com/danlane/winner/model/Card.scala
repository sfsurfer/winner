package com.danlane.winner.model

case class Card(face: CardFace, suit: Suit) {
  import Card._
  val value: Int = getValue(face, suit)
  override def toString = s"$face of ${suit}s"
}

object Card {
  def getValue(face: CardFace, suit: Suit): Int = face.singleValue * 4 + suit.value
}
