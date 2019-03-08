package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment}

trait Simulator[Env <: Environment[Action, A], A <: Agent, Action]{
  protected var board: Env
  val model1: PlayerModel[Env, A, Action]
  val model2: PlayerModel[Env, A, Action]
  var turn: Int
  val shouldApplyNoise: Boolean
  protected val P1TURN = 0
  protected val P2TURN = 1
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
  def runStep(): Option[(Env, Action, Env)] = {
    winner = determineWinner(board)
    if(winner.nonEmpty) return None
    val model = if(turn == P1TURN) model1 else model2
    val opposingPlayer = if(turn == P1TURN) board.p2 else board.p1
    val player = if(turn == P1TURN) board.p1 else board.p2
    val move = model.selectMove(player, board)
    val prevBoard = board
    board = transition(player, opposingPlayer, move, prevBoard)
    turn = if(turn == P1TURN) P2TURN else P1TURN
    Some(prevBoard, move, board)
  }

  def transition(agent1: A, agent2: A, action: Action, env: Env): Env

  def determineWinner(env: Env): Option[A]

  def getWinner: Option[A] = winner

  def isGameOver: Boolean = winner.nonEmpty
  def getBoard: Env = board
}
