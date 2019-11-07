package com.danlane.winner.model

sealed trait CardFace {
  def singleValue: Int
  def str8HiValue: Int = singleValue
  def str8LoValue: Int = str8HiValue
}

object CardFace {

  object Two extends CardFace {
    val singleValue = 14;
    override val str8HiValue = 1
    override def toString = "Two"
  }

  object Three extends CardFace {
    val singleValue = 2
    override def toString = "Three"
  }

  object Four extends CardFace {
    val singleValue = 3
    override def toString = "Four"
  }

  object Five extends CardFace {
    val singleValue = 4
    override def toString = "Five"
  }

  object Six extends CardFace {
    val singleValue = 5
    override def toString = "Six"
  }

  object Seven extends CardFace {
    val singleValue = 6
    override def toString = "Seven"
  }

  object Eight extends CardFace {
    val singleValue = 7
    override def toString = "Eight"
  }

  object Nine extends CardFace {
    val singleValue = 8
    override def toString = "Nine"
  }

  object Ten extends CardFace {
    val singleValue = 9
    override def toString = "Ten"
  }

  object Jack extends CardFace {
    val singleValue = 10
    override def toString = "Jack"
  }

  object Queen extends CardFace {
    val singleValue = 11
    override def toString = "Queen"
  }

  object King extends CardFace {
    val singleValue = 12
    override def toString = "King"
  }

  object Ace extends CardFace {
    val singleValue = 13;
    override val str8LoValue = 0
    override def toString = "Ace"
  }

  def allCardFaces: Vector[CardFace] = Vector(
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace
  )
}
