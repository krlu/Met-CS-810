package org.bu.met810

import org.bu.met810.data.BattleshipSim
import org.scalatest.{FlatSpec, Matchers}

class BattleshipTest extends FlatSpec with Matchers {
  "Battleship simulator" should "initialize random start state" in {
    val numPieces = 2
    val width = 5
    val height = 5
    for(_ <- 0 to 100) {
      val sim = BattleshipSim.randomInitialization(width, height, numPieces)
      val p1PiecePos = sim.board.p1.piecePositions
      val p2PiecePos = sim.board.p2.piecePositions
      List(p1PiecePos, p2PiecePos).foreach { positions =>
        assert(positions.forall { case (x, y) => x < width && y < height })
        assert(positions.size <= BattleshipSim.pieceLengths.max * numPieces)
      }
    }
  }
}
