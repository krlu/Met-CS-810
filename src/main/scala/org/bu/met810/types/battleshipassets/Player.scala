package org.bu.met810.types.battleshipassets

import org.bu.met810.types.{Agent, Vectorizable}

/**
  * @param positions - maps position to 1 or 0. 1 for alive 0 for dead
  * @param id - playerId
  * @param movesMade - maps position to -1, 0, or 1
  *                  1 = hit,
  *                  0 = miss,
  *                 -1 = move for this position not made yet
  */
sealed case class Player(positions: List[((Int, Int), Int)], id: Int,
                         movesMade: List[((Int, Int), Int)]) extends Agent with Vectorizable {

  private val DESTROYED_ID = 0
  def isDestroyed: Boolean = positions.forall(_._2 == DESTROYED_ID)

  override val state: Seq[Int] = positions.map{_._2} ++ movesMade.map{_._2}

  override val toVector: Seq[Double] = state.map(_.toDouble)
}

