package org.bu.met810.model

import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

trait PlayerModel{
  def selectMove(player: Player, board: Board): Move
}
