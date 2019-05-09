package org.bu.simmba.simulation

import org.bu.simmba.models.PlayerModel
import org.bu.simmba.types.{Agent, Environment}

trait Simulator [Env <: Environment[Action, A], A <: Agent, Action] {
  protected var board: Env
  val model1: PlayerModel[Env, A, Action]
  val model2: PlayerModel[Env, A, Action]
  protected var turn: Int
  val shouldApplyNoise: Boolean = false
  protected var winner: Option[A] = None

  def setBoard(newBoard: Env, playerId: Int): Unit = {
    winner = None
    board = newBoard
  }

  def determineWinner(env: Env): Option[A]

  def getWinner: Option[A] = winner
  def isGameOver: Boolean = winner.nonEmpty
  def getBoard: Env = board
}