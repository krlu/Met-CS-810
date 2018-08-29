package org.bu.met810.model

import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

trait NNPlayerModel extends PlayerModel{
  def vectorizeBoard(board: Board): Seq[Double]
  def vectorizeMovesForPlayer(player: Player): Seq[Double]
  def vectorToMove(vector: Seq[Double]): Move
}
