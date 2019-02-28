package org.bu.met810

import org.bu.met810.data.BattleshipSim
import org.scalatest.{FlatSpec, Matchers}

class BattleshipTest extends FlatSpec with Matchers {
  "Battleship simulator" should "initialize random start state" in {
    val numPieces = 2
    val boardSize = 5
    for(_ <- 0 to 100) {
      val sim = BattleshipSim.randomInitialization(envSize = boardSize)
      val p1PiecePos = sim.board.p1.positions
      val p2PiecePos = sim.board.p2.positions
      List(p1PiecePos, p2PiecePos).foreach { positions =>
        assert(positions.forall { case (x, y) => x < boardSize && y < boardSize })
        assert(positions.size <= BattleshipSim.pieceLengths.max * numPieces)
      }
    }
  }
}
