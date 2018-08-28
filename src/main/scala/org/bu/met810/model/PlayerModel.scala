package org.bu.met810.model

import org.bu.met810.types.moves.Move

trait PlayerModel{
  def selectMove(possibleMoves: Seq[Move]): Move
}
