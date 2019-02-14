package org.bu.met810.models.random

import org.bu.met810.choose
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}
import org.bu.met810.types.{Action, Environment}

class RandomMoveModel[Env <: Environment[A, Agent], Agent, A <: Action](moveSet: List[A]) extends PlayerModel[Env, Agent, A]{
  def selectMove(player: Agent, env: Env): A = choose(moveSet.filter(m => env.isValidAction(m, player)))
}

object RandomMoveModel{
  def crModel(moves: List[Move]) = new RandomMoveModel[Board, Player, Move](moves)
}