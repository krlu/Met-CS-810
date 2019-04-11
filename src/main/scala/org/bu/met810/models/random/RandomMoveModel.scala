package org.bu.met810.models.random

import org.bu.met810.choose
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment}
import org.bu.met810.types._

class RandomMoveModel[Env <: Environment[Action, A], A <: Agent, Action](moveSet: List[Action]) extends PlayerModel[Env, A, Action]{
  def selectMove(player: A, env: Env): Action = choose(moveSet.filter(m => env.isValidAction(m, player)))
  def modelName = "Random"
}

object RandomMoveModel{
  private type CrMove = copsandrobbersassets.Move
  private type CrBoard = copsandrobbersassets.Board
  private type CrPlayer = copsandrobbersassets.Player

  private type BsMove = battleshipassets.Move
  private type BsBoard = battleshipassets.Board
  private type BsPlayer = battleshipassets.Player

  def crModel(moves: List[CrMove]) = new RandomMoveModel[CrBoard, CrPlayer, CrMove](moves)
  def BShipModel(moves: List[BsMove]) = new RandomMoveModel[BsBoard, BsPlayer, BsMove](moves)
}
