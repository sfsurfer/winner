package com.danlane.winner.engine

import com.danlane.winner.model.PlayableHand.Pass
import com.danlane.winner.model.{Card, Deck, Hand, PlayableHand, Player}

case class Game(players: Vector[Player], deck: Deck)

/*
players, obvi
winner, if set we know who won
loser, if set, we know hand is over
currentTurn: index of players who goes next, needs to be reset often, on loss, set to loser's position, initialize to None, then set to 3 of diamonds
lastPlayerToPlay: index of last player to play a hand, can be used to determine when a hand is over.
currentPlay: current hand to beat, set to None on clearing
 */
case class GameState(
  players: Vector[Player],
  winner: Option[Player],
  loser: Option[Player],
  currentTurn: Option[Int],
  lastPlay: Option[Int],
  currentPlay: Option[PlayableHand]
) {

  val deck = Deck.create()
  def deal: Vector[Player] = deck.shuffle().deal(players.length).zipWithIndex.collect { case (hand, i) =>
    Player(players(i).name, Some(hand))
  }


}



/*
Game play
Hand:
 a) cards owned by player
 b) A set of rounds, where a winner and loser is determined. i.e. play a hand
Play:
 a) cards played by player
Round:
 a) A set of plays,

Game: A deal
Round: A lead
Play: Single play, e.g. a lead or a bomb



init -> shuffle and deal
initializeHand -> Swap if necessary, reorder players so winner or 3 of diamonds goes first

 */

object Game {

  val delimiter = " "

  val maxPlayPrompts = 20

  case class GameState(
                        players: Vector[Player],
                        winner: Option[Player],
                        loser: Option[Player],
                        currentTurn: Option[Int],
                        lastPlay: Option[Int],
                        currentPlay: Option[PlayableHand]
                      )

  case class GameResult(winner: Option[Player], loser: Option[Player])

  case class RoundResult(winner: Option[Player], players: Vector[Player], gameResult: GameResult)

  def init(players: Vector[Player]): Vector[Player] =
    Deck.create().shuffle().deal(players.length).zipWithIndex.collect { case (hand, i) =>
      Player(players(i).name, Some(hand))
    }

  def buildPlay(player: Player, play: String): Vector[Card] =
    play.split(delimiter).map(_.toInt).map(player.hand.map(_.cards).getOrElse(Vector.empty[Card])).toVector

  def getPlay(currentHand: PlayableHand, currentPlayer: Player): Option[PlayableHand] = {
    var result: Option[PlayableHand] = None
    (1 to maxPlayPrompts).toStream.takeWhile { _ =>
      println(currentHand.cards)
      val play = scala.io.StdIn.readLine("Enter cards to play or 'p' to pass: ")

      result = if (play == "p") {
        Some(Pass())
      } else {

        val newHand = buildPlay(currentPlayer, play)

        if (currentHand.isBeatenBy(newHand)) {
          PlayableHand.playHand(newHand)
        } else Option.empty[PlayableHand]
      }
      result.isEmpty
    }
    result
  }


  def playRound(players: Vector[Player], gameResult: GameResult = GameResult(None, None)): RoundResult = {
    var passes: Map[Int, Boolean] = players.zipWithIndex.map { case (_, i) => i -> false }.toMap
    var currentIdx: Int = 0
    var currentHand: PlayableHand = Pass()
    var winner = Option.empty[Player]
    var loser = Option.empty[Player]
    var mutPlayers = players
    while (passes.count(_._2 == true) < players.length - 1) {
      val currentPlayer = mutPlayers(currentIdx)
      val newPlay = getPlay(currentHand, currentPlayer) // TODO: Need to evaluate this...

      mutPlayers = mutPlayers.map {
        case p if p.name == currentPlayer.name =>
          currentPlayer.copy(hand = currentPlayer.hand.map(c => Hand(c.cards.diff(newPlay.get.cards))))
        case p => p
      }

      if (newPlay.contains(Pass()))
        passes = passes ++ Map(currentIdx -> true)
      else {
        passes = passes ++ Map(currentIdx -> false)
        if (gameResult.winner.isEmpty && mutPlayers(currentIdx).hand.exists(_.cards.isEmpty)) winner = Some(mutPlayers(currentIdx))
        if (mutPlayers.count(_.hand.exists(_.cards.isEmpty)) == players.length - 1) loser = mutPlayers.find(_.hand.exists(_.cards.nonEmpty))
        currentHand = newPlay.get // TODO: add checks?
      }
      currentIdx = (currentIdx + 1) % players.length
    }
    RoundResult(Some(mutPlayers(currentIdx)), mutPlayers, GameResult(winner, loser))
  }

  def winnerLoserSwap(gameResult: GameResult, winnerCard: Card): GameResult = {
    val losersHighestCard = gameResult.loser.flatMap(_.hand.flatMap(_.highestCard)).get
    val newWinnerHand = gameResult.winner.flatMap(_.hand.map(_.exchangeCard(losersHighestCard, winnerCard)))
    val newLoserHand = gameResult.loser.flatMap(_.hand.map(_.exchangeCard(winnerCard, losersHighestCard)))
    GameResult(
      gameResult.winner.map(_.copy(hand = newWinnerHand.map(_.sort))),
      gameResult.loser.map(_.copy(hand = newLoserHand.map(_.sort)))
    )
  }

  def initializeHand(players: Vector[Player], gameResult: Option[GameResult]): Vector[Player] =
    (for {
      results <- gameResult
      winnerNoHand <- results.winner
      winner <- players.find(_.name == winnerNoHand.name)
      loserNoHand <- results.loser
      loser <- players.find(_.name == loserNoHand.name)
      lowestCard <- winner.hand.flatMap(_.lowestCard)
      afterSwap = winnerLoserSwap(GameResult(Some(winner), Some(loser)), lowestCard)
    } yield {
      val updatedPlayers = players.map {
        case p if afterSwap.winner.exists(_.name == p.name) => afterSwap.winner.get
        case p if afterSwap.loser.exists(_.name == p.name) => afterSwap.loser.get
        case p => p
      }
      val startPosition = updatedPlayers.indexWhere(_.name == loser.name)
      reorderPlayers(updatedPlayers, startPosition)
    }).getOrElse {
      val startPosition = players.indexWhere(_.has3ofDiamonds())
      reorderPlayers(players, startPosition)
    }

  def reorderPlayers(players: Vector[Player], startPosition: Int): Vector[Player] =
    players.slice(startPosition, players.length) ++ players.slice(0, startPosition)

  //case class RoundResult(winner: Player, players: Vector[Player], gameResult: GameResult)
  def playGame(players: Vector[Player]): Unit = {
    var keepPlaying = false //true
    val result = Option.empty[GameResult]
    while (keepPlaying) {
      val initializedPlayers = initializeHand(init(players), result)
      // TODO: Look into refactoring this
      var roundResult: RoundResult = playRound(initializedPlayers)
      while (roundResult.gameResult.loser.isEmpty) {
        roundResult = playRound(reorderPlayers(players, players.indexWhere(_.name == roundResult.winner.get.name)))
      }
      println(s"WINNER: ${roundResult.gameResult.winner}")
      println(s"LOSER: ${roundResult.gameResult.loser}")
      // TODO: Fix RoundResult to have optional winner?
      keepPlaying = {
        val in = scala.io.StdIn.readLine("Keep playing? (yes/no)")
        in == "yes" || in == "y"
      }
    }
  }
}


object Round {
  type Passes = Vector[Boolean]

  def play(players: Vector[Player]): Vector[Player] = {
    val passes = (1 to players.length).map(_ => false).toVector
//    val lead = players.head.playAny
    while(roundVictor(players, passes).isEmpty) {
      players.head
    }
    players
  }

  def roundVictor(players: Vector[Player], passes: Passes): Option[Player] =
    if (passes.count(_ == true) == players.length - 1) Some(players(passes.indexOf(false)))
    else None
}
