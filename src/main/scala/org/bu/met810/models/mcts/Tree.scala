package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment, Vectorizable}

class Tree[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action]
(sim: Simulator[Env, A, Action], possibleMoves: Set[Action]) extends PlayerModel[Env, A , Action]{
  val root: Node[(Env, Action)] = null

  def playout(state: Env, move: Action, player: Agent, numPlayouts: Int = 1000): Int = {
    (0 to numPlayouts).flatMap { _ =>
      sim.setBoard(state)
      sim.runFullGame()
    }.count(_.id == player.id)
  }

  override def selectMove(agent: A, e: Env): Action = {
    root.children.maxBy(_.value).move._2
  }
}
