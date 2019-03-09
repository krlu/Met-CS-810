package org.bu.met810.types.battleshipassets

import org.bu.met810.types.{Agent, Vectorizable}

sealed case class Player(positions: List[((Int, Int), Int)], id: Int,
                         movesMade: Map[(Int, Int), Int]) extends Agent with Vectorizable {

  def isDestroyed: Boolean = positions.forall(_._2 == 0)

  override val state: Seq[Int] = positions.flatMap{ case((x,y), isAlive) => List(x,y,isAlive)
  } ++ movesMade.toList.flatMap{case ((x,y), moveMade) => List(x,y,moveMade)}

  override val toVector: Seq[Double] = state.map(_.toDouble)
}

