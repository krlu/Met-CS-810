package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator
import org.bu.met810.types.{Action, Environment}

class Tree[Env <: Environment[A, Agent], Agent, A <: Action](sim : Simulator[Env, Agent, A], possibleMoves: Set[A]){
  private var currentState: Env = sim.board
  private var storedStates: Set[Env] = Set.empty[Env]

}
