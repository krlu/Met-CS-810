package org.bu.simmba.simulation

import org.bu.simmba.types.{Agent, Environment}

/**
  * Typically used for (but not limited to) turn based games like battleships, chess, checkers
  * @tparam Env - Must extend Environment trait
  * @tparam A - Must extend Agent trait
  * @tparam Action - Can be anything (for now)
  */
trait TurnBasedSimulator[Env <: Environment[Action, A], A <: Agent, Action] extends Simulator[Env, A, Action]{
  protected var board: Env
  protected var turn: Int
  protected val P1TURN = 0
  protected val P2TURN = 1

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
    val player = if(turn == P1TURN) board.p1 else board.p2
    val move = model.selectMove(player, board)
    val prevBoard = board
    board = transition(player, move, prevBoard)
    turn = if(turn == P1TURN) P2TURN else P1TURN
    Some(prevBoard, move, board)
  }

  override def setBoard(newBoard: Env, playerId: Int): Unit = {
    turn = playerId
    winner = None
    board = newBoard
  }
  def getTurn: Int = turn

  def transition(agent1: A, action: Action, env: Env): Env
  def determineWinner(env: Env): Option[A]
}

