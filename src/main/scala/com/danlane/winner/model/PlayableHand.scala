package com.danlane
package winner
package model

trait PlayableHand {
  def cards: Vector[Card]

  // Returns true if cards passed in beats this hand
  def isBeatenBy(other: Vector[Card]): Boolean = other match {
    case c if PlayableHand.Bomb.validate(c) => true
    case c if validate(c) => c.maxBy(_.value).value > cards.maxBy(_.value).value
    case _ => false
  }

  def validate(cards: Vector[Card]): Boolean
}

object PlayableHand {
  sealed case class Pass() extends PlayableHand {
    val cards = Vector.empty[Card]
    override def validate(cards: Vector[Card]): Boolean = cards.isEmpty
    override def isBeatenBy(other: Vector[Card]): Boolean = true
  }

  sealed abstract case class Single(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = Single.validate(cards)
  }
  object Single {
    def validate(cards: Vector[Card]): Boolean = cards.length == 1
  }

  sealed abstract case class Pair(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = Pair.validate(cards)
  }
  object Pair {
    def validate(cards: Vector[Card]): Boolean = cards.length == 2 && cards.count(_.face == cards.head.face) == 2
  }

  sealed abstract case class Trip(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = Trip.validate(cards)
  }
  object Trip {
    def validate(cards: Vector[Card]): Boolean = cards.length == 3 && cards.count(_.face == cards.head.face) == 3
  }

  /** BOMB */
  sealed abstract case class Bomb(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = Bomb.validate(cards)

    override def isBeatenBy(other: Vector[Card]): Boolean = other match {
      case c if validate(c) => c.head.value > cards.head.value
      case _ => false
    }
  }
  object Bomb {
    def validate(cards: Vector[Card]): Boolean =  cards.length == 4 && cards.count(_.face == cards.head.face) == 4
  }

  /** STRAIGHT */
  sealed abstract case class Straight(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = Straight.validate(cards)
    // Returns true if cards passed in beats this hand
    override def isBeatenBy(other: Vector[Card]): Boolean = other match {
      case c if Bomb.validate(c) => true
      case c if c.length != cards.length => false
      case c if StraightFlush.validate(c) => true
      case c if Straight.validate(c) => Straight.getHighCardValue(c) > Straight.getHighCardValue(this.cards)
      case _ => false
    }
  }
  object Straight {
    def validate(cards: Vector[Card]): Boolean = cards match {
      case c if c.count(_.face == CardFace.Ace) == 2 => isStraight(c.sortBy(_.face.str8LoValue).tail) // Remove first Ace
      case c if c.count(_.face == CardFace.Two) == 1 => isStraight(c.sortBy(_.face.str8LoValue))
      case _ => isStraight(cards.sortBy(_.face.str8HiValue))
    }
    def getHighCardValue(cards: Vector[Card]): Int = if (validate(cards)) {
      cards match {
        case c if c.count(_.face == CardFace.Ace) == 2 => CardFace.Ace.str8HiValue
        case c if c.count(_.face == CardFace.Two) == 1 => c.maxBy(_.face.str8LoValue).face.str8LoValue
        case _ => cards.maxBy(_.face.str8HiValue).face.str8HiValue
      }
    } else 0
  }

  /** STRAIGHT FLUSH */
  sealed abstract case class StraightFlush(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = StraightFlush.validate(cards)

    override def isBeatenBy(other: Vector[Card]): Boolean = other match {
      case c if Bomb.validate(c) => true
      case c if StraightFlush.validate(c) && c.length == cards.length =>
        Straight.getHighCardValue(c) > Straight.getHighCardValue(cards)
      case _ => false
    }
  }
  object StraightFlush {
    def validate(cards: Vector[Card]): Boolean = isStraight(cards) && cards.count(_.suit == cards.head.suit) == cards.length
  }

  /** DOUBLE STRAIGHT */
  sealed abstract case class DoubleStraight(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = DoubleStraight.validate(cards)

    override def isBeatenBy(other: Vector[Card]): Boolean = other match {
      case c if Bomb.validate(c) => true
      case c if validate(c) && c.length == cards.length =>
        DoubleStraight.getHighCardValue(c) > DoubleStraight.getHighCardValue(cards)
      case _ => false
    }
  }

  object DoubleStraight {
    def validate(cards: Vector[Card]): Boolean = {
      cards match {
        case c if cards.count(_.face == CardFace.Two) == 2 => validateDoubleStraight(c.sortBy(_.face.str8LoValue))
        case c => validateDoubleStraight(c.sortBy((_.face.str8HiValue)))
      }
    }
    def getHighCardValue(cards: Vector[Card]): Int = if (validate(cards)) {
      cards match {
        case c if c.count(_.face == CardFace.Two) == 2 => c.maxBy(_.face.str8LoValue).face.str8LoValue
        case _ => cards.maxBy(_.face.str8HiValue).face.str8HiValue
      }
    } else 0
  }

  private[model] def validateDoubleStraight(cards: Vector[Card]): Boolean = {
    cards.length >= 6 &&
      cards.grouped(2).zipWithIndex.map { case (c, i) =>
        c.length == 2 && c(0).face == c(1).face &&
          (c(0).face.str8LoValue == cards.head.face.str8LoValue + i ||
            c(0).face.str8HiValue == cards.head.face.str8LoValue + i)
      }.forall(_ == true)
  }

  /** FULL HOUSE  */
  sealed abstract case class FullHouse(cards: Vector[Card]) extends PlayableHand {
    override def validate(cards: Vector[Card]): Boolean = FullHouse.validate(cards)
    override def isBeatenBy(other: Vector[Card]): Boolean = other match {
      case c if Bomb.validate(c) => true
      case c if validate(c) => new FullHouse(c) {}.value > value
    }

    val value: Int = cards.groupBy(_.face).filter(_._2.length == 3).keys.head.singleValue
  }
  object FullHouse {
    def validate(cards: Vector[Card]): Boolean =
      Option(cards.partition(_.face == cards.head.face)).exists {
        case (x, y) if x.length == 2 => Pair.validate(x) && Trip.validate(y)
        case (x, y) if x.length == 3 => Trip.validate(x) && Pair.validate(y)
        case _ => false
      }
  }

  /** INSTANTIATION */
  def playHand(cards: Vector[Card]): Option[PlayableHand] = {
    cards match {
      case c if StraightFlush.validate(c) => Some(new StraightFlush(c) {})
      case c if Straight.validate(c) => Some(new Straight(c) {})
      case c if DoubleStraight.validate(c) => Some(new DoubleStraight(c) {})
      case c if FullHouse.validate(c) => Some(new FullHouse(c) {})
      case c if Bomb.validate(c) => Some(new Bomb(c) {})
      case c if Trip.validate(c) => Some(new Trip(c) {})
      case c if Pair.validate(c) => Some(new Pair(c) {})
      case c if Single.validate(c) => Some(new Single(c) {})
      case _ => None
    }
  }

  def isStraight(cards: Vector[Card]): Boolean = {
    cards.length >= 3 && cards.zipWithIndex.map { case (card, i) =>
      card.face.str8LoValue == cards.head.face.str8LoValue + i ||
        card.face.str8HiValue == cards.head.face.str8LoValue + i
    }.forall(_ == true)
  }
}
