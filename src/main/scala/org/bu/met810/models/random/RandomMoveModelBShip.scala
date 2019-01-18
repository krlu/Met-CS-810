package org.bu.met810.models.random

import org.bu.met810.types.battleshipassets.{Board, Move, Player}

class RandomMoveModelBShip extends RandomMoveModel[Board, Player, Move] {
  override def moveSet(player: Player, board: Board): List[Move] = {
    {for{
      x <- 0 until board.width
      y <- 0 until board.height
    } yield Move(x,y)}.toList.filter(m => !board.movesMade(player.id).contains(m))
  }
  override def getPlayer(playerId: Int, board: Board): Option[Player] =
    Set(board.p1, board.p2).find(_.id == playerId)
}

object RandomMoveModelBShip {
  def apply(): RandomMoveModelBShip = new RandomMoveModelBShip
}