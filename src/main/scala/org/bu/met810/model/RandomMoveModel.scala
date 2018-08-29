package org.bu.met810.model

import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves._


/**
  * Selects a move at uniform random from a set of possible moves
  */
class RandomMoveModel extends PlayerModel {
  override def selectMove(player: Player, board: Board): Move = choose(player.moves.iterator)
  private def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1
}
