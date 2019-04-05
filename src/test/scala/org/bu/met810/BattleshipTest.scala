package org.bu.met810

import org.bu.met810.data.{BattleshipSim, DataGenerator}
import org.bu.met810.models.generative.NNPlayerModel
import org.bu.met810.models.mcts.MCTS
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.battleshipassets.{Board, Move, Player}
import org.scalatest.{FlatSpec, Matchers}

class BattleshipTest extends FlatSpec with Matchers {
  "Battleship simulator" should "initialize random start state" in {
    val numPieces = 2
    val boardSize = 5
    val dim = 2
    val numPlayers = 2
    val posVectors = possiblePositions(boardSize,boardSize,dim)
    val possibleMoves = posVectors.map{ pos => Move(pos.head, pos(1))}

    def vectorToBoard(vector: Seq[Int]): Board = {
      def vectorToPlayer(playerVector: Seq[Int], id: Int): Player = {
        val (posStatuses, moveMadeStatus) = playerVector.splitAt(playerVector.size/2)
        val positions = (posVectors zip posStatuses).map{ case (pos, status) => (pos.head, pos(1)) -> status}
        val movesMade = (posVectors zip moveMadeStatus).map{ case (pos, status) => (pos.head, pos(1)) -> status}
        Player(positions, id, movesMade)
      }
      val playerDataVector = vector.dropRight(2)
      val dimensions = vector.takeRight(2)
      val (p1Vector, p2Vector) = playerDataVector.splitAt(playerDataVector.length/2)
      val p1 = vectorToPlayer(p1Vector, 0)
      val p2 = vectorToPlayer(p2Vector, 1)
      Board(p1, p2, dimensions.head, dimensions(1))
    }

    val model1 = RandomMoveModel.BShipModel(possibleMoves)
    for(_ <- 0 to 100) {
      val sim = BattleshipSim.randomInitialization(model1, model1, envSize = boardSize)
      val p1PiecePos = sim.getBoard.p1.positions
      val p2PiecePos = sim.getBoard.p2.positions
      List(p1PiecePos, p2PiecePos).foreach { positions =>
        assert(positions.forall { case ((x, y), _) => x < boardSize && y < boardSize })
        assert(positions.count(_._2 == 1) <= BattleshipSim.pieceLengths.max * numPieces)
        assert(positions.size == boardSize * boardSize)
      }
      val vec = sim.getBoard.toVector
      assert(vec.size == boardSize*boardSize*2*numPlayers + 2)
      assert(vectorToBoard(vec.map(_.toInt)) == sim.getBoard)
    }
  }


  "Random BS Ship Models" should "win equally" in {
    val boardSize = 5
    val moveDim = 2
    def vectorToMove(vector: Seq[Int]): Move = Move(vector.head, vector(1))
    val possibleMoves = possiblePositions(boardSize, boardSize, moveDim).map{vectorToMove}
    val sim = BattleshipSim.randomInitialization(
      RandomMoveModel.BShipModel(possibleMoves),
      RandomMoveModel.BShipModel(possibleMoves),
      boardSize
    )
    val p1Model = new MCTS(sim, possibleMoves)

    val winners = BattleshipSim.runBatch(
      RandomMoveModel.BShipModel(possibleMoves),
      p1Model,
      envSize = 5)
    val p1Wins = winners.count(_.id == 0)
    val p2Wins =  winners.count(_.id == 1)
//    assert(Math.abs(p1Wins.toDouble/(p1Wins + p2Wins) - 0.5) < 0.01)
    println(p1Wins, p2Wins)
  }
}
