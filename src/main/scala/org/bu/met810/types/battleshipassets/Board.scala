package org.bu.met810.types.battleshipassets

import org.bu.met810.types.Environment

sealed case class Board(p1: Player, p2: Player, width: Int, height: Int, movesMade: Map[Int, Set[Move]] = Map()) extends Environment[Move, Player] {
  override def isValidAction(action: Move, agent: Player): Boolean = ???
}