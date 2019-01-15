package org.bu.met810.types.battleshipassets

case class Player(initialPiecePositions: List[(Int, Int)]){
  def removePiece(pos: (Int, Int)): Player = {
    Player(initialPiecePositions.filter{p => p == pos})
  }
}
