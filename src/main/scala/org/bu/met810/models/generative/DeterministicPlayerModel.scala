package org.bu.met810.models.generative

import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

class DeterministicPlayerModel(val paramsFile: String, val useGenerativeParams: Boolean)
  extends PlayerModel[Board, Player, Move] with JsonModelLoader with BoardValidation{
  override def selectMove(playerId: Int, board: Board): Move = {
    val player: Player = Set(board.p1, board.p2).find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    val (x1, y1) = board.p1.position
    val (x2, y2) = board.p2.position
    val key = s"${playerId}_${List(x1,y1,x2,y2).mkString("_")}_move"
    val deterministicBestMove = (paramsMap(key) zip player.moves).filter{ case (_, m) =>
      validMoves(player, board).contains(m)
    }.sortWith(_._1 > _._1).head._2
    deterministicBestMove
  }
}
