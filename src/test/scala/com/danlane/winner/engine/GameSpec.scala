package com.danlane.winner.engine

import com.danlane.winner.model._
import com.danlane.winner.ui.UI
import org.scalatest.{FlatSpec, Matchers}

class GameSpec extends FlatSpec with Matchers {
  import CardFace._
  import Suit._

  val players = Vector(
    Player("a"),
    Player("b"),
    Player("c"),
    Player("d")
  )

  val initState = GameState(
    players,
    None,
    None,
    None,
    None,
    None
  )

  val postGameState = GameState(
    players,
    Some(1),
    Some(2),
    Some(0),
    Some(0),
    PlayableHand.playHand(Vector(Card(Two, Diamond)))
  )

  def validateFreshDeal(state: GameState) = {
    state.players.map { p =>
      p.hand.map { h => h.cards.length should be (13) }
    }
  }

  val mockUI = new UI {

    def update(state: GameState): Unit = ()

    def getNextPlay(state: GameState): Option[PlayableHand] = PlayableHand.playHand(Vector(Card(Three, Diamond)))

    def getWinnersSwapCard(state: GameState): Option[Card] = state.winner.flatMap(_.hand.flatMap(_.lowestCard))
  }

  val game = new Game(mockUI)

  "deal" should "deal cards to players" in {
    val dealtState = game.deal(initState)
    validateFreshDeal(dealtState)
    dealtState.loser shouldBe None
    dealtState.winner shouldBe None
    dealtState.currentTurn shouldBe None
    dealtState.currentPlay shouldBe None
    dealtState.lastToPlay shouldBe None
  }

  "initialize hand" should "set player with 3 of diamonds first when no loser in state" in {
    val dealtState = game.initializeHand(initState)
    val with3OfDiamonds = dealtState.players.indexWhere { p => p.has3ofDiamonds() }
    dealtState.currentTurn shouldBe Some(with3OfDiamonds)
    dealtState.loser shouldBe None
    dealtState.winner shouldBe None
    dealtState.currentPlay shouldBe None
    dealtState.lastToPlay shouldBe None
  }

  it should "set loser to go first if loser is not empty" in {
    val loser = postGameState.loserIdx
    val dealtState = game.initializeHand(postGameState)
    validateFreshDeal(dealtState)
    dealtState.currentTurn shouldBe loser
    dealtState.loser shouldBe None
    dealtState.winner shouldBe None
    dealtState.currentPlay shouldBe None
    dealtState.lastToPlay shouldBe None
  }

  it should "swap losers highest and winners selected card" in {
    val dealtState = game.deal(postGameState)
    for {
      loser <- postGameState.loserIdx
      winner <- postGameState.winnerIdx
      losersHighest <- dealtState.players(loser).hand.flatMap(_.highestCard)
      winnersLowest <- dealtState.players(winner).hand.flatMap(_.lowestCard)
    } yield {
      val postSwapState = game.winnerLoserSwap(dealtState)
      postSwapState.players(loser).hand.exists(_.cards.contains(winnersLowest)) shouldBe true
      postSwapState.players(winner).hand.exists(_.cards.contains(losersHighest)) shouldBe true
    }
  }
}
