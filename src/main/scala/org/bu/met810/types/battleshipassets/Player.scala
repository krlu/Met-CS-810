package org.bu.met810.types.battleshipassets

import org.bu.met810.types.{Agent, Vectorizable}

/**
  * @param positions - maps position to 1 or 0. 1 for alive 0 for dead
  * @param id - playerId
  * @param movesMade - maps position to -1, 0, or 1. 1 for hit, 0 for miss, -1 for move for this position not made yet
  */
sealed case class Player(positions: Map[(Int, Int), Int], id: Int,
                         movesMade: Map[(Int, Int), Int]) extends Agent with Vectorizable {

  private val DESTROYED_ID = 0
  def isDestroyed: Boolean = positions.forall(_._2 == DESTROYED_ID)

  override val state: Seq[Int] = positions.toList.flatMap{ case((x,y), isAlive) => List(x,y,isAlive)
  } ++ movesMade.toList.flatMap{case ((x,y), moveMade) => List(x,y,moveMade)}

  override val toVector: Seq[Double] = state.map(_.toDouble)
}

