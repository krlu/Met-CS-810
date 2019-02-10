package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator

class Tree[Env, Agent, Action](sim : Simulator[Env, Agent, Action], possibleMoves: Set[Action]){
  private var currentState: Env = sim.board
  private var storedStates: Set[Env] = Set.empty[Env]

}
