package org.bu.met810.models.random

import org.bu.met810.choose
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.Environment
import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}

class RandomMoveModel[Env <: Environment[Action, Agent], Agent, Action](moveSet: List[Action]) extends PlayerModel[Env, Agent, Action]{
  def selectMove(player: Agent, env: Env): Action = choose(moveSet.filter(m => env.isValidAction(m, player)))
}

object RandomMoveModel{
  def crModel(moves: List[Move]) = new RandomMoveModel[Board, Player, Move](moves)
}