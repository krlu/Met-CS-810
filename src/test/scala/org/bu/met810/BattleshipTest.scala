package org.bu.met810

import org.bu.met810.data.BattleshipSim
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.battleshipassets.{Board, Move, Player}
import org.scalatest.{FlatSpec, Matchers}

class BattleshipTest extends FlatSpec with Matchers {
  "Battleship simulator" should "initialize random start state" in {
    val numPieces = 2
    val boardSize = 5
    val dim = 2
    val possibleMoves = possibleStates(boardSize,boardSize,dim).map{ pos =>
      Move(pos.head, pos(1))
    }
    val model1 = new RandomMoveModel[Board, Player, Move](possibleMoves)
    for(_ <- 0 to 100) {
      val sim = BattleshipSim.randomInitialization(model1, model1, envSize = boardSize)
      val p1PiecePos = sim.getBoard.p1.positions
      val p2PiecePos = sim.getBoard.p2.positions
      List(p1PiecePos, p2PiecePos).foreach { positions =>
        assert(positions.forall { case ((x, y), _) => x < boardSize && y < boardSize })
        assert(positions.count(_._2 == 1) <= BattleshipSim.pieceLengths.max * numPieces)
        assert(positions.size == boardSize * boardSize)
      }
    }
  }

  "Random BS Ship Models" should "win equally" in {
    val boardSize = 5
    val moveDim = 2
    def vectorToMove(vector: Seq[Int]): Move = Move(vector.head, vector(1))
    val possibleMoves = possibleStates(boardSize, boardSize, moveDim).map{vectorToMove}
    val winners = BattleshipSim.runBatch(
      RandomMoveModel.BShipModel(possibleMoves),
      RandomMoveModel.BShipModel(possibleMoves), envSize = 5, numTrials = 10000)
    val p1Wins = winners.count(_.id == 0)
    val p2Wins =  winners.count(_.id == 1)
    assert(Math.abs(p1Wins.toDouble/(p1Wins + p2Wins) - 0.5) < 0.01)
    println(p1Wins, p2Wins)
  }
}
