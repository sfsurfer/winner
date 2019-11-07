package com.danlane.winner.model

import org.scalatest._
import scala.util.Random

class PlayableHandSpec extends FlatSpec with Matchers {
  import CardFace._
  import Suit._
  import PlayableHand._

  def randSuit = allSuits(Random.nextInt(allSuits.length))

  def genStraight(start: CardFace, len: Int, isFlush: Boolean = false, flushSuit: Suit = Diamond, suitOffset: Int = 0): Vector[Card] = {
    val (startVec, startIdx) = if (start == Ace) {
      (Vector(Ace), 0)
    } else { (Vector.empty[CardFace], allCardFaces.indexOf(start)) }

    val faces = startVec ++ allCardFaces.slice(startIdx, startIdx + (len - startVec.length))

    faces.zipWithIndex.map { case (face, i) =>
      if (isFlush) {
        Card(face, flushSuit)
      } else {
        Card(face, Suit.allSuits((i + suitOffset) % 4))
      }
    }
  }

  def genDoubleStraight(start: CardFace, len: Int, isFlush: Boolean = false, flushSuit: (Suit, Suit) = (Diamond, Club)): Vector[Card] = {
    genStraight(start, len, isFlush, flushSuit._1) ++ genStraight(start, len, isFlush, flushSuit._2, 1)
  }
  def genFullHouse(tripFace: CardFace, pairFace: CardFace): Vector[Card] =
    Suit.allSuits.take(3).map(Card(tripFace, _)) ++ Suit.allSuits.take(2).map(Card(pairFace, _))

  "genStraight" should "build a straight" in {
    val cards = genStraight(Three, 3)
    cards.length should be(3)
    cards.count(_.face.str8HiValue == 2) should be(1)
    cards.count(_.face.str8HiValue == 3) should be(1)
    cards.count(_.face.str8HiValue == 4) should be(1)
    Suit.allSuits.map { suit => cards.count(_.suit == suit) should be < 3 }
  }

  "genStraight" should "build a straight starting at ace low" in {
    val cards = genStraight(Ace, 3)
    cards.length should be(3)
    cards.count(_.face.str8LoValue == 0) should be(1)
    cards.count(_.face.str8LoValue == 1) should be(1)
    cards.count(_.face.str8LoValue == 2) should be(1)
    Suit.allSuits.map { suit => cards.count(_.suit == suit) should be < 3 }
  }
  "genStraight" should "build a straight flush with default suit" in {
    val cards = genStraight(Three, 3, true)
    cards.length should be(3)
    cards.count(_.face.str8HiValue == 2) should be(1)
    cards.count(_.face.str8HiValue == 3) should be(1)
    cards.count(_.face.str8HiValue == 4) should be(1)
    cards.count(_.suit == Diamond) should be(3)
  }
  "genStraight" should "build a straight flush with custom suit" in {
    val cards = genStraight(Three, 3, true, Spade)
    cards.length should be(3)
    cards.count(_.face.str8HiValue == 2) should be(1)
    cards.count(_.face.str8HiValue == 3) should be(1)
    cards.count(_.face.str8HiValue == 4) should be(1)
    cards.count(_.suit == Spade) should be(3)
  }
  "genDoubleStraight" should "build a double straight" in {
    val cards = genDoubleStraight(Three, 3)
    cards.length should be(6)
    (Three.str8HiValue to Five.str8HiValue).map { v => cards.count(_.face.str8HiValue == v) should be(2) }
    Suit.allSuits.map { suit => cards.count(_.suit == suit) should be < 3 }
  }
  "genDoubleStraight" should "build a double straight flush" in {
    val cards = genDoubleStraight(Three, 3, true, (Heart, Spade))
    cards.length should be(6)
    (Three.str8HiValue to Five.str8HiValue).map { v => cards.count(_.face.str8HiValue == v) should be(2) }
    cards.count(_.suit == Heart) should be(3)
    cards.count(_.suit == Spade) should be(3)
  }

  val single = Vector(Card(Ace, Diamond))
  val pair = Vector(
    Card(Ace, Diamond),
    Card(Ace, Heart)
  )
  val trips = Vector(
    Card(Ace, Diamond),
    Card(Ace, Club),
    Card(Ace, Heart)
  )
  val bomb = Vector(
    Card(Ace, Diamond),
    Card(Ace, Spade),
    Card(Ace, Club),
    Card(Ace, Heart)
  )
  val fullHouse = Vector(
    Card(King, Club),
    Card(King, Diamond),
    Card(Ace, Spade),
    Card(Ace, Club),
    Card(Ace, Heart)
  )
  val doubleStraightAceLow = Vector(
    Card(Ace, Diamond),
    Card(Ace, Club),
    Card(Two, Diamond),
    Card(Two, Spade),
    Card(Three, Diamond),
    Card(Three, Heart),
  )
  val doubleStraightAceHigh = Vector(
    Card(Queen, Diamond),
    Card(Queen, Club),
    Card(King, Diamond),
    Card(King, Spade),
    Card(Ace, Diamond),
    Card(Ace, Heart),
  )
  val straightAce2Ace = Vector(
    Card(Ace, Diamond),
    Card(Two, Spade),
    Card(Three, Diamond),
    Card(Four, Club),
    Card(Five, Diamond),
    Card(Six, Heart),
    Card(Seven, Diamond),
    Card(Eight, Spade),
    Card(Nine, Diamond),
    Card(Ten, Club),
    Card(Jack, Diamond),
    Card(Queen, Heart),
    Card(King, Diamond),
    Card(Ace, Spade),
  )
  val straight2toAce = Vector(
    Card(Two, Diamond),
    Card(Three, Heart),
    Card(Four, Diamond),
    Card(Five, Spade),
    Card(Six, Diamond),
    Card(Seven, Club),
    Card(Eight, Diamond),
    Card(Nine, Heart),
    Card(Ten, Diamond),
    Card(Jack, Spade),
    Card(Queen, Diamond),
    Card(King, Spade),
    Card(Ace, Diamond)
  )

  "A Single Play" should "be valid for a single card" in {
    val cards = Vector(Card(Ace, Diamond))
    Single.validate(cards) should be(true)
  }
  "Playing a single card" should "return a Single" in {
    val cards = Vector(Card(Ace, Diamond))
    playHand(cards).map(_ shouldBe a [Single])
  }
  "Playing a pair card" should "return a Pair" in {
    playHand(pair).map(_ shouldBe a[Pair])
  }
  "Playing trips" should "return a Trip" in {
    playHand(trips).map(_ shouldBe a[Trip])
  }
  "Playing bomb" should "return a Bomb" in {
    playHand(bomb).map(_ shouldBe a[Bomb])
  }
  "Playing full house" should "return a FullHouse" in {
    playHand(fullHouse).map(_ shouldBe a[FullHouse])
  }

  "Playing a double straight with ace low" should "return a DoubleStraight" in {
    playHand(doubleStraightAceLow) match {
      case Some(_: DoubleStraight) => info("Correct hand returned")
      case Some(a) => fail(s"Got wrong type of hand, expecting DoubleStraight, got ${a}")
      case None => fail("No hand found, expecting DoubleStraight")
    }
  }
  "Playing a double straight with ace high" should "return a DoubleStraight" in {
    playHand(doubleStraightAceHigh) match {
      case Some(_: DoubleStraight) => info("Correct hand returned")
      case Some(a) => fail(s"Got wrong type of hand, expecting DoubleStraight, got ${a}")
      case None => fail("No hand found, expecting DoubleStraight")
    }
  }
  "Playing a straight ace to ace" should "return a Straight" in {
    playHand(straightAce2Ace) match {
      case Some(_: Straight) => info("Correct hand returned")
      case Some(a) => fail(s"Got wrong type of hand, expecting Straight, got ${a}")
      case None => fail("No hand found, expecting Straight")
    }
  }
  "Playing a straight 2 to ace" should "return a Straight" in {
    playHand(straight2toAce) match {
      case Some(_: Straight) => info("Correct hand returned")
      case Some(a) => fail(s"Got wrong type of hand, expecting Straight, got ${a}")
      case None => fail("No hand found, expecting Straight")
    }
  }

  "Invalid hands" should "be not be playable" in {
    info("No cards")
    isNotPlayable(Vector.empty[Card])
    info("2 card straight")
    isNotPlayable(Vector(Card(Three, Club), Card(Four, Heart)))
    info("Pair with extra card")
    isNotPlayable(Vector(Card(Three, Spade), Card(Four, Heart), Card(Four, Club)))
    info("2 card double straight")
    isNotPlayable(Vector(Card(Three, Heart), Card(Three, Spade), Card(Four, Heart), Card(Four, Club)))
  }

  "Playing a 2 carded straight" should "fail" in {
    playHand(straight2toAce) match {
      case Some(_: Straight) => info("Correct hand returned")
      case Some(a) => fail(s"Got wrong type of hand, expecting Straight, got ${a}")
      case None => fail("No hand found, expecting Straight")
    }
  }

  // -- TESTING isBeatenBy -- //
  "A higher single" should "beat a lower single" in {
    playHand(single).map(_.isBeatenBy(Vector(Card(Two, Spade)))) should be (Some(true))
  }
  "A higher single by suit only" should "beat a lower single" in {
    playHand(Vector(Card(Two, Heart))).map(_.isBeatenBy(Vector(Card(Two, Spade)))) should be (Some(true))
  }
  "A lower single" should "not beat a higher single" in {
    playHand(single).map(_.isBeatenBy(Vector(Card(Three, Diamond)))) should be (Some(false))
  }
  "A lower single" should "not beat a higher single by suit only" in {
    playHand(Vector(Card(Three, Club))).map(_.isBeatenBy(Vector(Card(Three, Diamond)))) should be (Some(false))
  }

  "A higher pair" should "beat a lower pair" in {
    playHand(pair).map(_.isBeatenBy(Vector(Card(Two, Club),Card(Two, Spade)))) should be (Some(true))
  }
  "A higher pair by suit only" should "beat a lower pair" in {
    playHand(pair).map(_.isBeatenBy(Vector(Card(Ace, Club), Card(Ace, Spade)))) should be (Some(true))
  }
  "A lower pair" should "not beat a higher pair" in {
    playHand(pair).map(_.isBeatenBy(Vector(Card(Three, Diamond), Card(Three, Heart)))) should be (Some(false))
  }
  "A lower pair" should "not beat a higher pair by suit only" in {
    playHand(Vector(Card(Ace, Club), Card(Ace, Spade))).map(_.isBeatenBy(pair)) should be (Some(false))
  }

  "A higher trip" should "beat a lower trip" in {
    playHand(trips).map(_.isBeatenBy(Vector(Card(Two, Diamond), Card(Two, Club), Card(Two, Spade)))) should be (Some(true))
  }
  "A lower trip" should "not beat a higher trip" in {
    playHand(trips).map(_.isBeatenBy(Vector(Card(Three, Diamond), Card(Three, Club), Card(Three, Heart)))) should be (Some(false))
  }
  "An invalid hand" should "not beat a higher trip" in {
    playHand(trips).map(_.isBeatenBy(Vector(Card(Two, Diamond), Card(Two, Club), Card(Ace, Spade)))) should be (Some(false))
  }

  // STRAIGHTS
  "A higher straight" should "beat a lower straight of same number of cards" in {
    playHand(genStraight(Three, 3)).map(_.isBeatenBy(genStraight(Four, 3))) should be(Some(true))
  }
  "A higher straight to ace" should "beat a lower straight" in {
    playHand(genStraight(Three, 3)).map(_.isBeatenBy(genStraight(Queen, 3))) should be(Some(true))
  }
  "A higher straight to ace" should "not be beaten by a lower straight" in {
    playHand(genStraight(Queen, 3)).map(_.isBeatenBy(genStraight(Three, 3))) should be(Some(false))
  }
  "A higher straight" should "beat a straight of ace low" in {
    playHand(genStraight(Ace, 3)).map(_.isBeatenBy(genStraight(Three, 3))) should be(Some(true))
  }
  "A higher straight" should "beat a higher straight of two low" in {
    playHand(genStraight(Two, 3)).map(_.isBeatenBy(genStraight(Three, 3))) should be(Some(true))
  }
  "A straight with two low" should "not beat a higher straight of same number of cards" in {
    playHand(genStraight(Three, 3)).map(_.isBeatenBy(genStraight(Two, 3))) should be(Some(false))
  }
  "A straight with ace low" should "not beat a higher straight of same number of cards" in {
    playHand(genStraight(Three, 3)).map(_.isBeatenBy(genStraight(Ace, 3))) should be(Some(false))
  }
  "A straight to same value" should "not beat straight" in {
    playHand(genStraight(Four, 3)).map(_.isBeatenBy(genStraight(Four, 3))) should be (Some(false))
  }
  "A lower straight flush" should "beat a higher non flush straight" in  {
    playHand(genStraight(Ten, 3)).map(_.isBeatenBy(genStraight(Four, 3, true))) should be (Some(true))
  }
  "A higher non flush straight flush" should "not beat a lower striaght flush" in  {
    playHand(genStraight(Three, 3, true)).map(_.isBeatenBy(genStraight(Ten, 3))) should be (Some(false))
  }
  "A straight of different length" should "not win" in  {
    playHand(genStraight(Three, 3)).map(_.isBeatenBy(genStraight(Four, 4))) should be (Some(false))
  }
  "A straight" should "lose to a bomb" in  {
    playHand(genStraight(Three, 3)).map(_.isBeatenBy(bomb)) should be (Some(true))
  }

  // DOUBLE STRAIGHTS
  "A higher double straight" should "beat a lower double straight" in {
    playHand(genDoubleStraight(Three, 3)).map(_.isBeatenBy(genDoubleStraight(Ten, 3))) should be(Some(true))
  }
  it should "beat a lower double straight with ace low" in {
    playHand(genDoubleStraight(Ace, 3)).map(_.isBeatenBy(genDoubleStraight(Ten, 3))) should be(Some(true))
  }
  it should "beat a lower double straight with 2 low" in {
    playHand(genDoubleStraight(Two, 3)).map(_.isBeatenBy(genDoubleStraight(Ten, 3))) should be(Some(true))
  }
  it should "win when ace high" in {
    playHand(genDoubleStraight(Three, 3)).map(_.isBeatenBy(genDoubleStraight(Queen, 3))) should be (Some(true))
  }
  "A lower double straight" should "not beat a higher double straight" in {
    playHand(genDoubleStraight(Ten, 3)).map(_.isBeatenBy(genDoubleStraight(Three, 3))) should be(Some(false))
  }
  it should "not win when ace is low" in {
    playHand(genDoubleStraight(Ten, 3)).map(_.isBeatenBy(genDoubleStraight(Ace, 3))) should be(Some(false))
  }
  it should "not win when 2 is low" in {
    playHand(genDoubleStraight(Ten, 3)).map(_.isBeatenBy(genDoubleStraight(Two, 3))) should be(Some(false))
  }
  it should "not win when to the same face value" in {
    playHand(genDoubleStraight(Ten, 3)).map(_.isBeatenBy(genDoubleStraight(Ten, 3))) should be(Some(false))
  }
  it should "not win when different lengths" in {
    playHand(genDoubleStraight(Three, 3)).map(_.isBeatenBy(genDoubleStraight(Seven, 4))) should be (Some(false))
  }
  it should "lose to a bomb" in {
    playHand(genDoubleStraight(Three, 3)).map(_.isBeatenBy(bomb)) should be (Some(true))
  }

  // FULL HOUSES
  "A stronger house" should "beat a lower house" in {
    val house1 = genFullHouse(Four, Five)
    val house2 = genFullHouse(King, Queen)
    playHand(house1).map(_.isBeatenBy(house2)) should be (Some(true))
  }
  "A house of 2s" should "beat any other houser" in {
    val house1 = genFullHouse(Ace, Five)
    val house2 = genFullHouse(Two, Queen)
    playHand(house1).map(_.isBeatenBy(house2)) should be (Some(true))
  }
  "A house of Aces" should "beat most houses" in {
    val house1 = genFullHouse(King, Five)
    val house2 = genFullHouse(Ace, Queen)
    playHand(house1).map(_.isBeatenBy(house2)) should be (Some(true))
  }
  "A house with higher pair" should "not beat higher house" in {
    val house1 = genFullHouse(King, Five)
    val house2 = genFullHouse(Queen, Two)
    playHand(house1).map(_.isBeatenBy(house2)) should be (Some(false))
  }
  "A house with higher pair" should "lose to a bomb" in {
    val house = genFullHouse(King, Five)
    val bomb = Suit.allSuits.map(Card(Three, _))
    playHand(house).map(_.isBeatenBy(bomb)) should be (Some(true))
  }

  // BOMBS
  "A higher bomb" should "beat a lower bomb" in {
    val bomb1 = Suit.allSuits.map(Card(Three, _))
    val bomb2 = Suit.allSuits.map(Card(Four, _))
    playHand(bomb1).map(_.isBeatenBy(bomb2)) should be (Some(true))
  }
  "A bomb of 2s" should "beat a lower bomb" in {
    val bomb1 = Suit.allSuits.map(Card(King, _))
    val bomb2 = Suit.allSuits.map(Card(Two, _))
    playHand(bomb1).map(_.isBeatenBy(bomb2)) should be (Some(true))
  }
  "A bomb of 2s" should "beat a bomb of Aces" in {
    val bomb1 = Suit.allSuits.map(Card(Ace, _))
    val bomb2 = Suit.allSuits.map(Card(Two, _))
    playHand(bomb1).map(_.isBeatenBy(bomb2)) should be (Some(true))
  }
  "A bomb of Aces" should "beat a lower bomb" in {
    val bomb1 = Suit.allSuits.map(Card(King, _))
    val bomb2 = Suit.allSuits.map(Card(Ace, _))
    playHand(bomb1).map(_.isBeatenBy(bomb2)) should be (Some(true))
  }

  def isNotPlayable(cards: Vector[Card]): Unit = {
    Single.validate(cards) should be(false)
    Pair.validate(cards) should be(false)
    Trip.validate(cards) should be(false)
    Bomb.validate(cards) should be(false)
    Straight.validate(cards) should be(false)
    StraightFlush.validate(cards) should be(false)
    DoubleStraight.validate(cards) should be(false)
    FullHouse.validate(cards) should be(false)
  }
}
