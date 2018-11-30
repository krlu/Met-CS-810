package org.bu.met810.models.generative

import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move
import org.bu.met810.applyNoise


/**
  * Predicts discrete events via un-supervised learning by building a complete graph over all possible events
  */

class DeterministicPlayerModel(val paramsFile: String, val useGenerativeParams: Boolean)
  extends PlayerModel[Board, Player, Move] with JsonModelLoader with BoardValidation{
  override def selectMove(playerId: Int, board: Board): Move = {
    val player: Player = Set(board.p1, board.p2).find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    val (x1, y1) = board.p1.position
    val possibleP2Positions =
      applyNoise(board.p2.position, positionRadius = 1, minFactor = 0.5)
        .filter(p => validPosition(p._2, board) && p._2 != player.position)
    possibleP2Positions.flatMap{ case (_, (x2, y2)) =>
      val key = s"${playerId}_${List(x1,y1,x2,y2).mkString("_")}_move"
      (paramsMap(key) zip player.moves).filter{ case (_, m) =>
        validMoves(player, board).contains(m)
      }
    }.sortWith(_._1 > _._1).head._2
  }
}

object DeterministicPlayerModel{
  def apply(paramsFile: String, useGenerativeParams: Boolean): DeterministicPlayerModel =
    new DeterministicPlayerModel(paramsFile,useGenerativeParams)
}
