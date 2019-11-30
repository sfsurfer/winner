package com.danlane.winner.model

case class Hand(cards: Vector[Card]) {

  val lowestCard: Option[Card] = if (cards.nonEmpty) Some(cards.minBy(_.value)) else None
  val highestCard: Option[Card] = if (cards.nonEmpty) Some(cards.maxBy(_.value)) else None

  def playCards(that: Vector[Card]): Vector[Card] =
    if (cards.intersect(that) == that) {
      cards.diff(that)
    } else Vector.empty[Card]

  def exchangeCard(newCard: Card, oldCard: Card): Hand =
    Hand(cards.diff(Vector(oldCard)) :+ newCard).sort

  def sort: Hand = Hand(cards.sortBy(_.value))

  override def toString: String = "\n\t" + cards.map(_.toString).mkString("\n\t")
  def toStringWithIndex: String = "\n\t" + cards.zipWithIndex.map{ case (c,i) => s"${i.toString}: ${c.toString}" }.mkString("\n\t")
}

object Hand {
  def empty = Hand(Vector.empty[Card])
}
