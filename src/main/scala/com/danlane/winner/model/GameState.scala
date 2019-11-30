package com.danlane.winner
package model

import scala.util.Try


case class GameState(
  // Players, order is important as index of player is used frequently
  players: Vector[Player],
  // If set, game has a winner. Resets to None on card exchange
  winnerIdx: Option[Int],
  // If set, game is over and loser will have to exchange cards. Resets to None on card exchange
  loserIdx: Option[Int],
  // Index of Player who's turn it is currently
  currentTurn: Option[Int],
  // Index of Player who made the last play. When equal to currentTurn, round is over
  lastToPlay: Option[Int],
  // The current play. Resets to None after currentTurn == lastPlay
  currentPlay: Option[PlayableHand]
)

object GameState {
  implicit class GameStateOps(state: GameState) {

    def winner: Option[Player] = state.winnerIdx.map(state.players)

    def loser: Option[Player] = state.loserIdx.map(state.players)

    def currentPlayer: Option[Player] = state.currentTurn.map(state.players)

    def playerAtIdx(i: Int): Option[Player] = Try { state.players(i) }.toOption

    def playerIdx(player: Player): Option[Int] = state.players.indexWhere(_.name == player.name) match {
      case i if i < state.players.length && i >= 0 => Some(i)
      case _ => None
    }
  }
}

