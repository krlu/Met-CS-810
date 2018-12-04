package org.bu.met810.models.generative

import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move


/**
  * Predicts discrete events via un-supervised learning by building a complete graph over all possible events
  */
class DeterministicPlayerModel(val paramsFile: String, val useGenerativeParams: Boolean)
  extends PlayerModel[Board, Player, Move] with JsonModelLoader with BoardValidation{
  override def selectMove(playerId: Int, board: Board): Move = {
    val (player, possiblePositions) = getPlayerData(playerId, board)
    val (x1, y1) = player.position
    possiblePositions.head.flatMap{ case (_, (x2, y2)) =>
      val queryString = s"${playerId}_${List(x1,y1,x2,y2).mkString("_")}_move"
      (paramsMap(queryString) zip player.moves).filter{ case (_, m) =>
        validMoves(player, board).contains(m)
      }
    }.sortWith(_._1 > _._1).head._2
  }
}

object DeterministicPlayerModel{
  def apply(paramsFile: String, useGenerativeParams: Boolean): DeterministicPlayerModel =
    new DeterministicPlayerModel(paramsFile,useGenerativeParams)
}
