package org.bu.met810.types.battleshipassets

import org.bu.met810.types.Agent

sealed case class Player(positions: List[(Int, Int)], id: Int) extends Agent{
  def removePiece(pos: (Int, Int)): Player = {
    Player(positions.filter{ p => p == pos}, id)
  }
}
