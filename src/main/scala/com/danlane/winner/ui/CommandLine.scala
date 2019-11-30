package com.danlane.winner
package ui

import com.danlane.winner.model.PlayableHand.Pass
import com.danlane.winner.model._

import scala.util.Try

class CommandLine extends UI {

  import CommandLine._

  override def update(state: GameState): Unit = {
    println("Players: ")
    state.players.foreach { p => println(s" Player ${p.name} had ${p.hand.map(_.cards.length).getOrElse(0)} cards") }
    println(s"Current play is ${state.currentPlay.getOrElse("None")}")
    state.currentPlayer.foreach(p => println(s"Player ${p.name}'s turn"))
  }

  override def displayPlay(play: PlayableHand): Unit = println(s"Played Hand:\n\t$play")

  override def gameOverDisplay(state: GameState): GameState = {
    println(s"Winner = ${state.winner.map(_.name)}")
    println(s"Loser = ${state.loser.map(_.name)}")
    state
  }

  /** TODO: Add some kind of retry here */
  override def getWinnersSwapCard(state: GameState): Option[Card] =
    state.winner.flatMap(_.hand).flatMap { hand =>
      Try(scala.io.StdIn.readLine(PromptWinnerForCard(hand)).trim.toInt).toOption.map(hand.cards)
    }

  def playHand(state: GameState, play: Vector[Card]): Option[PlayableHand] =
    state.currentPlay match {
      case Some(currPlay) if currPlay.isBeatenBy(play) => PlayableHand.playHand(play)
      case Some(_) => None
      case None => PlayableHand.playHand(play).flatMap { p =>
        println(NicePlay)
        Some(p)
      } orElse {
        println(InvalidPlay)
        None
      }
    }

  override def getNextPlay(state: GameState): Option[PlayableHand] =
    state.currentPlayer.flatMap { player =>
      update(state)
      player.hand.flatMap { hand =>
        promptForPlay(hand, state.currentPlay) match {
          case play if state.currentPlay.isDefined && play == "p" => Some(Pass())
          case play =>
            buildPlay(player, play) match {
              case Some(newPlay) =>
                playHand(state, newPlay) match {
                  case None => getNextPlay(state)
                  case p => p
                }
              case None => println("Bad play! Try again"); getNextPlay(state)
            }
        }
      }
    }

  override def showSwappedCards(winnersCard: Card, losersCard: Card): Unit = {
    println(s"Winner's Card: ${winnersCard}")
    println(s"Loser's Card: ${losersCard}")
  }

  override def promptToContinue: Boolean =
    (1 to maxPlayPrompts).foldLeft(Option.empty[Boolean]){ (a, _) =>
      a orElse {
        scala.io.StdIn.readLine(PromptToContinue).toLowerCase() match {
          case "y" | "yes" => Some(true)
          case "n" | "no" => Some(false)
          case _ =>
            println("Unrecognized response, please respond 'y' or 'n'")
            None
        }
      }
    }.getOrElse(false)

  def promptForPlay(hand: Hand, currentPlay: Option[PlayableHand] = None): String =
    currentPlay match {
      case Some(_) => scala.io.StdIn.readLine(PromptForPlay(hand))
      case _ => scala.io.StdIn.readLine(PromptForLead(hand))
    }

  def buildPlay(player: Player, play: String): Option[Vector[Card]] = Try {
    play.split(Delimiter).map(_.toInt).map(player.hand.map(_.cards).getOrElse(Vector.empty[Card])).toVector.sortBy(_.value)
  }.toOption
}

object CommandLine {
  private[winner] val Delimiter = " "

  private[winner] val maxPlayPrompts = 20

  val YourHand: Hand => String = hand =>
    s"Your hand: \n${hand.toStringWithIndex}"

  val PromptForLead: Hand => String = hand => s"${YourHand(hand)}\nEnter lead play: "
  val PromptForPlay: Hand => String = hand =>  s"${YourHand(hand)}\nEnter cards to play or 'p' to pass: "
  val PrintHand: PlayableHand => String = hand => s"Current hand to beat: ${hand}"

  val PromptWinnerForCard: Hand => String = hand =>
    s"${YourHand(hand)}\nSelect card to swap with loser: "

  val NicePlay = "Nice Play!"

  val PlayDoesNotWin: (Vector[Card], PlayableHand) => String = (played, toBeat) =>
    s"$played does not beat $toBeat. Please try again"

  val InvalidPlay = "Not a valid play, please try again"

  private[winner] val PromptToContinue = "Play another round? [y/n]"
}
