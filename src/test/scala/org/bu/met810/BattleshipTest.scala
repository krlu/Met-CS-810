package org.bu.met810

import java.io.FileWriter

import org.bu.met810.data.BattleshipSim
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

  "Random BS Ship Models" should "win with varying amounts" in {
    val fw = new FileWriter("battleship_results.csv", true)
    fw.write("p1 Model, p2 Model, p1Wins, p2Wins, boardSize, numPlayouts, winPct, computing time\n")
    def vectorToMove(vector: Seq[Int]): Move = Move(vector.head, vector(1))
    def constructRandomModel(possibleMoves: List[Move], boardSize: Int): BattleshipSim =
      BattleshipSim.randomInitialization(
        RandomMoveModel.BShipModel(possibleMoves),
        RandomMoveModel.BShipModel(possibleMoves),
        boardSize
      )
    for{
      boardSize <- List(10, 20)
      numPlayouts <- List(100,250, 500, 1000)
    }{
      val moveDim = 2
      val possibleMoves = possiblePositions(boardSize, boardSize, moveDim).map(vectorToMove)
      val sim1 = constructRandomModel(possibleMoves, boardSize)
      val sim2 = constructRandomModel(possibleMoves, boardSize)
      for {
        p1Model <- List(MCTS(sim1, possibleMoves, numPlayouts = numPlayouts))
        p2Model <- List(RandomMoveModel.BShipModel(possibleMoves), MCTS(sim2, possibleMoves, numPlayouts = numPlayouts))
      }{
        val start = System.currentTimeMillis()
        val winners = BattleshipSim.runBatch(p1Model, p2Model, envSize = boardSize)
        val p1Wins = winners.count(_.id == 0)
        val p2Wins = winners.count(_.id == 1)
        val winPct = p1Wins.toDouble / (p1Wins + p2Wins)
        val end = System.currentTimeMillis()
        val time = (end - start).toDouble/1000
        val results = List(p1Model.modelName, p2Model.modelName, p1Wins, p2Wins, boardSize, numPlayouts, winPct, time)
        println(results.mkString(","))
        fw.write( results.mkString(",") + "\n")
      }
    }
    fw.close()
  }
}
