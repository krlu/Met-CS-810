package org.bu.met810.types.battleshipassets

sealed case class Player(piecePositions: List[(Int, Int)], id: Int){
  def removePiece(pos: (Int, Int)): Player = {
    Player(piecePositions.filter{ p => p == pos}, id)
  }
}
