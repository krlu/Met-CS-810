package org.bu.met810.data

import org.bu.met810.models.PlayerModel

trait Simulator[Env, Agent, Action]{
  var board: Env
  val model1: PlayerModel[Env, Agent, Action]
  val model2: PlayerModel[Env, Agent, Action]
  var turn: Int
  val shouldApplyNoise: Boolean
  protected var winner: Option[Agent] = None

  /**
    * Runs the simulation from start to finish
    * @return
    */
  def runFullGame(maxCounter: Int = 1000): Option[Agent] = {
    var counter = 0
    while (winner.isEmpty && counter < maxCounter) {
      runSimulator()
      counter += 1
    }
    winner
  }
  /**
    * Performs one turn update in the simulation
    * @return
    */
  def runSimulator(): Option[(Env, Action, Env)]

  def getWinner: Option[Agent] = winner

  def isGameOver: Boolean = winner.nonEmpty
}
