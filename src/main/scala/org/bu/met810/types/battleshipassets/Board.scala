package org.bu.met810.types.battleshipassets

import org.bu.met810.types.{Environment, Vectorizable}

sealed case class Board(p1: Player, p2: Player,
                        width: Int, height: Int,
                        movesMade: Map[Int, Set[Move]] = Map()) extends Environment[Move, Player] with Vectorizable{
  override def isValidAction(action: Move, agent: Player): Boolean = ???

  override val id: Int = ???
  override val toVector: Seq[Double] = ???
}