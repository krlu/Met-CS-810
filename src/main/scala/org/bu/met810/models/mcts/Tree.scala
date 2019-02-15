package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator
import org.bu.met810.types.{Environment, Vectorizable}

class Tree[Env <: Environment[Action, Agent] with Vectorizable, Agent, Action](sim : Simulator[Env, Agent, Action], possibleMoves: Set[Action]){
  private var currentState: Env = sim.board
  private var storedStates: Set[Env] = Set.empty[Env]

}
