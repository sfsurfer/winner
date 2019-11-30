package com.danlane.winner
package ui

import com.danlane.winner.model._

trait UI {
  def update(state: GameState): Unit

  def getNextPlay(state: GameState): Option[PlayableHand]

  def displayPlay(play: PlayableHand): Unit

  def gameOverDisplay(state: GameState): GameState

  def getWinnersSwapCard(state: GameState): Option[Card]

  def showSwappedCards(winnersCard: Card, losersCard: Card): Unit

  def promptToContinue: Boolean
}
