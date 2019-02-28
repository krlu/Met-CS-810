package org.bu.met810.models.random

import org.bu.met810.choose
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment}
import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}

class RandomMoveModel[Env <: Environment[Action, A], A <: Agent, Action](moveSet: List[Action]) extends PlayerModel[Env, A, Action]{
  def selectMove(player: A, env: Env): Action = choose(moveSet.filter(m => env.isValidAction(m, player)))
}

object RandomMoveModel{
  def crModel(moves: List[Move]) = new RandomMoveModel[Board, Player, Move](moves)
}