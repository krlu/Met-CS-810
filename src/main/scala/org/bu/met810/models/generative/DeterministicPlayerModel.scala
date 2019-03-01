package org.bu.met810.models.generative

import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.{Agent, Environment}


/**
  * Predicts discrete events via un-supervised learning by building a complete graph over all possible events
  */
class DeterministicPlayerModel[Env <: Environment[Action, A], A <: Agent, Action]
(val paramsFile: String, val useGenerativeParams: Boolean, possibleMoves: Seq[Action])
  extends PlayerModel[Env, A, Action] with JsonModelLoader with BoardValidation{

  override def selectMove(player: A, board: Env): Action = {
    val (x1, y1) = player.positions.head
    val opposingPlayer = if(player == board.p1) board.p2 else board.p1
    val (x2, y2) = opposingPlayer.positions.head
    val queryString = s"${player.id}_${List(x1,y1,x2,y2).mkString("_")}_move"
    (paramsMap(queryString) zip possibleMoves).filter{ case (_, m) => board.isValidAction(m,player)}
  }.maxBy(_._1)._2
}

object DeterministicPlayerModel{
  def apply[Env <: Environment[Action, A], A <: Agent, Action]
  (paramsFile: String, useGenerativeParams: Boolean, possibleMoves: Seq[Action]) =
    new DeterministicPlayerModel[Env, A, Action](paramsFile,useGenerativeParams, possibleMoves)
}
