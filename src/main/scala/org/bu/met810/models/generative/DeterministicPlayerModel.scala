package org.bu.met810.models.generative

import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}


/**
  * Predicts discrete events via un-supervised learning by building a complete graph over all possible events
  */
class DeterministicPlayerModel(val paramsFile: String, val useGenerativeParams: Boolean)
  extends PlayerModel[Board, Player, Move] with JsonModelLoader with BoardValidation{
  override def selectMove(player: Player, board: Board): Move = {
    val possiblePositions = List(player.position)
    val (x1, y1) = player.position
    val (x2, y2) = possiblePositions.head
    val queryString = s"${player.id}_${List(x1,y1,x2,y2).mkString("_")}_move"
    (paramsMap(queryString) zip player.moves).filter{ case (_, m) => validMoves(player, (board.width, board.length)).contains(m)}
  }.maxBy(_._1)._2
}

object DeterministicPlayerModel{
  def apply(paramsFile: String, useGenerativeParams: Boolean): DeterministicPlayerModel =
    new DeterministicPlayerModel(paramsFile,useGenerativeParams)
}
