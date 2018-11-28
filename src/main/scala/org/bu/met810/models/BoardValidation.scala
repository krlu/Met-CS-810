package org.bu.met810.models

import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

trait BoardValidation {
  def validMoves(player: Player, board: Board): List[Move] = player.moves.filter{ m: Move =>
    val (x1, y1) = m(player.position._1, player.position._2)
    x1 >= 0 && x1 < board.width && y1 >= 0 && y1 < board.length
  }

  def validPosition(pos: (Int, Int), board: Board): Boolean = {
    val (x1, y1) = pos
    x1 >= 0 && x1 < board.width && y1 >= 0 && y1 < board.length
  }
}
