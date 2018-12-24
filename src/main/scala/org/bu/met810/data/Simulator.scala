package org.bu.met810.data

import org.bu.met810.models.PlayerModel

trait Simulator[Env, Agent, Action]{
  var board: Env
  private val MAX_COUNTER = 1000
  val model1: PlayerModel[Env, Agent, Action]
  val model2: PlayerModel[Env, Agent, Action]
  var turn: Int
  private var counter = 0
  protected var winner: Option[Agent] = None

  /**
    * Runs the simulation from start to finish
    * @return
    */
  def runFullGame(): Option[Agent] = {
    while (winner.isEmpty && counter < MAX_COUNTER) {
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
