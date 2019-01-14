package org.bu.met810.models

import org.bu.met810.choose
import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}

/**
  * Selects a move at uniform random from a set of possible moves
  */
class RandomMoveModel extends PlayerModel[Board, Player, Move] with BoardValidation {
  override def selectMove(playerId: Int, board: Board): Move = {
    val player = Set(board.p1, board.p2).find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    choose(validMoves(player, board).iterator)
  }
}

object RandomMoveModel{
  def apply(): RandomMoveModel = new RandomMoveModel()
}
