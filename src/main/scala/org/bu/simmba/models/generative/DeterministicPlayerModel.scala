package org.bu.simmba.models.generative

import org.bu.simmba.models.{JsonModelLoader, PlayerModel}
import org.bu.simmba.types.{Agent, Environment}

/**
  * Predicts discrete events via un-supervised learning by building a complete graph over all possible events
  */
class DeterministicPlayerModel[Env <: Environment[Action, A], A <: Agent, Action]
(val paramsFile: String, val useGenerativeParams: Boolean, possibleMoves: Seq[Action])
  extends PlayerModel[Env, A, Action] with JsonModelLoader{

  override def selectMove(player: A, board: Env): Action = {
    val p1State = board.p1.state
    val p2State = board.p2.state
    val queryString = s"${(List(player.id) ++ p1State ++ p2State).mkString("_")}_move"
    (paramsMap(queryString) zip possibleMoves).filter{ case (_, m) => board.isValidAction(m,player)}
  }.maxBy(_._1)._2

  override def modelName: String = "Deterministic"
}

object DeterministicPlayerModel{
  def apply[Env <: Environment[Action, A], A <: Agent, Action]
  (paramsFile: String, useGenerativeParams: Boolean, possibleMoves: Seq[Action]) =
    new DeterministicPlayerModel[Env, A, Action](paramsFile,useGenerativeParams, possibleMoves)
}
