package com.danlane.winner.model

case class Deck(cards: Vector[Card]) {
  def shuffle(): Deck = Deck(scala.util.Random.shuffle(cards))

  def deal(hands: Int): Vector[Hand] = (0 until hands).map { hand =>
    Hand(cards.slice(hand,cards.length).zipWithIndex.collect{ case (card, i) if i % hands == 0 => card }.sortBy(_.value))
  }.toVector
}

object Deck {
  def create(): Deck = Deck(CardFace.allCardFaces.flatMap { face =>
    Suit.allSuits.map { suit =>
      Card(face, suit)
    }
  })

}
