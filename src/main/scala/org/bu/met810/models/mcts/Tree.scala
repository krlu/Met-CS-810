package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator
import org.bu.met810.types.{Agent, Environment, Vectorizable}

class Tree[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action](sim : Simulator[Env, A, Action], possibleMoves: Set[Action]){
  private var currentState: Env = sim.board
  private var storedStates: Set[Env] = Set.empty[Env]

}
