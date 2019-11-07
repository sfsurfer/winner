package com.danlane.winner.model

import org.scalatest.{FlatSpec, Matchers}

class CardSpec extends FlatSpec with Matchers {

  import CardFace._
  import Suit._

  "value" should "return correct value of a card" in {
    val card = Card(Three, Diamond)
    card.value should be(8)
  }
  "getValue" should "return correct value of a card" in {
    Card.getValue(Three, Diamond) should be(8)
  }
}
