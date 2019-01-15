package org.bu.met810.models.random

import org.bu.met810.models.BoardValidation
import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}

/**
  * Selects a move at uniform random from a set of possible moves
  */
class RandomMoveModelCR extends RandomMoveModel[Board, Player, Move] with BoardValidation {
  override def moveSet(player: Player, board: Board): List[Move] = validMoves(player, board)
  override def getPlayer(playerId: Int, board: Board): Option[Player] =
    Set(board.p1, board.p2).find(_.id == playerId)
}

object RandomMoveModelCR{
  def apply(): RandomMoveModelCR = new RandomMoveModelCR()
}