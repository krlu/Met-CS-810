package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment}

trait Simulator[Env <: Environment[Action, A], A <: Agent, Action]{
  var board: Env
  val model1: PlayerModel[Env, A, Action]
  val model2: PlayerModel[Env, A, Action]
  var turn: Int
  val shouldApplyNoise: Boolean
  protected var winner: Option[A] = None

  /**
    * Runs the simulation from start to finish
    * @return
    */
  def runFullGame(maxCounter: Int = 1000): Option[A] = {
    var counter = 0
    while (!isGameOver && counter < maxCounter) {
      runStep()
      counter += 1
    }
    winner
  }
  /**
    * Performs one turn update in the simulation
    * @return
    */
  def runStep(): Option[(Env, Action, Env)]

  def getWinner: Option[A] = winner

  def isGameOver: Boolean = winner.nonEmpty
}
