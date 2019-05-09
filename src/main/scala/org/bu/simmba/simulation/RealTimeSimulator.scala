package org.bu.simmba.simulation

import org.bu.simmba.types.{Agent, Environment}

trait RealTimeSimulator [Env <: Environment[Action, A], A <: Agent, Action] extends Simulator [Env, A, Action ]{

  /**
    * Runs the simulation from start to finish
    * @return
    */
  def runFullGame(maxCounter: Int = 1000, stepTime: Int = 0): Option[A] = {
    var counter = 0
    while (!isGameOver && counter < maxCounter) {
      if(stepTime > 0)
        Thread.sleep(stepTime)
      runStep()
      counter += 1
    }
    winner
  }

  /**
    * Performs one turn update in the simulation
    * @return
    */
  def runStep(): Option[(Env, Action, Action, Env)] = {
    winner = determineWinner(board)
    if(winner.nonEmpty) return None
    val move1 = model1.selectMove(board.p1, board)
    val move2 = model2.selectMove(board.p2, board)
    val prevBoard = board
    board = transition(board.p1, move1, board.p2, move2, prevBoard)
    Some(prevBoard, move1, move2, board)
  }

  def transition(agent1: A, action1: Action, agent2: A, action2: Action, env: Env): Env
}
