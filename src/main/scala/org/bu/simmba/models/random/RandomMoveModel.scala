package org.bu.simmba.models.random

import org.bu.simmba.choose
import org.bu.simmba.models.PlayerModel
import org.bu.simmba.types.{Agent, Environment}
import org.bu.simmba.types._

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
