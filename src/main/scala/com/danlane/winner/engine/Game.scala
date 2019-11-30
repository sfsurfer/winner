package com.danlane.winner
package engine

import com.danlane.winner.model.PlayableHand.Pass
import com.danlane.winner.model._
import com.danlane.winner.ui.UI

class Game(ui: UI) {

  /**
   * Main entry point
   * Given list of players, create initial GameState
   * After each round, prompt user to continue or not
   *
   * @param players
   */
  def startGame(players: Vector[Player]): Unit = {
    val initialState = GameState(
      players = players,
      winnerIdx = None,
      loserIdx = None,
      currentTurn = None,
      lastToPlay = None,
      currentPlay = None
    )
    playGame(initialState)
  }

  /**
   * Tail recursion
   *
   * @param state
   * @return
   */
  def playGame(state: GameState): GameState = {
    if (state.loser.isEmpty || ui.promptToContinue) {
      playGame(playRound(initializeHand(state)))
    } else state
  }

  def initializeHand(state: GameState): GameState = {
    val dealt = deal(state)
    val swapped = winnerLoserSwap(dealt)
    val goesFirst = swapped match {
      case s if s.winner.isEmpty => s.players.indexWhere(_.has3ofDiamonds())
      case s if s.loser.nonEmpty => s.players.indexWhere(p => s.loser.exists(_ === p))
      case _ => 0
    }
    swapped.copy(
      winnerIdx = None,
      loserIdx = None,
      currentPlay = None,
      currentTurn = Some(goesFirst),
      lastToPlay = None
    )
  }

  def playRound(state: GameState): GameState =
    if (state.loser.isEmpty) playRound(playHand(state))
    else ui.gameOverDisplay(state)

  def deal(state: GameState): GameState =
    state.copy(players = Deck.create().shuffle().deal(state.players.length).zipWithIndex.collect { case (hand, i) =>
      Player(state.players(i).name, Some(hand))
    })

  def updatePlayer(players: Vector[Player], newPlayer: Player): Vector[Player] =
    players.map { p =>
      if (p.name == newPlayer.name) newPlayer else p
    }

  def playHand(state: GameState): GameState =
    state.currentTurn.map(state.players).flatMap { player =>
      if (player.hasNoCards) Some(skipFinishedPlayers(state))
      else {
        ui.getNextPlay(state).map { play =>
          updateStateAfterPlay(state, player, play)
        }
      }
    }.getOrElse(state)

  def skipFinishedPlayers(state: GameState): GameState = {
    val iState = state.copy(currentTurn = getNextTurn(state))
    iState.copy(currentPlay = setCurrentPlay(iState, iState.currentPlay))
  }

  def checkForWinner(state: GameState): Option[Int] = state.winnerIdx orElse
    state.players.find(_.hasNoCards).flatMap(state.playerIdx)

  def checkForLoser(state: GameState): Option[Int] = state.players.filter(_.hasCards) match {
    case playing if playing.length == 1 => state.playerIdx(playing.head)
    case _ => None
  }

  def updateStateAfterPlay(state: GameState, player: Player, nextPlay: PlayableHand): GameState = {
    ui.displayPlay(nextPlay)
    val newPlayer = player.copy(hand = player.hand.map(h => Hand(h.playCards(nextPlay.cards))))
    val lastToPlay = if (nextPlay == Pass()) state.lastToPlay else state.playerIdx(player)
    val iState = state.copy(players = updatePlayer(state.players, newPlayer), currentTurn = getNextTurn(state), lastToPlay = lastToPlay)
    iState.copy(winnerIdx = checkForWinner(iState), loserIdx = checkForLoser(iState), currentPlay = setCurrentPlay(iState, Some(nextPlay)))
  }

  def setCurrentPlay(state: GameState, play: Option[PlayableHand]): Option[PlayableHand] = {
    ui.update(state)
    if (state.lastToPlay == state.currentTurn) None
    else play.flatMap (_ match {
      case _: Pass => state.currentPlay
      case _ => play
    }) orElse state.currentPlay
  }

  private[engine] def getNextTurn(state: GameState): Option[Int] =
    state.currentTurn.map(_ + 1).filter(_ < state.players.length).orElse(Some(0))

  /**
   * Swap cards
   * If no winner (e.g. first hand) just return state
   *
   * @param state
   * @return
   */
  def winnerLoserSwap(state: GameState): GameState =
    (for {
      winnersCard <- ui.getWinnersSwapCard(state)
      losersCard <- state.loser.flatMap(_.hand.flatMap(_.highestCard))
    } yield {
      ui.showSwappedCards(winnersCard, losersCard)
      val newPlayers = state.players.map {
        case p if state.winner.exists(_ === p) => p.copy(hand = p.hand.map(_.exchangeCard(losersCard, winnersCard)))
        case p if state.loser.exists(_ === p) => p.copy(hand = p.hand.map(_.exchangeCard(winnersCard, losersCard)))
        case p => p
      }
      state.copy(players = newPlayers)
    }).getOrElse(state)

}
