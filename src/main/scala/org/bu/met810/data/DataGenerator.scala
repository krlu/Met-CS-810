package org.bu.met810.data

import java.io.{File, FileWriter}

import org.bu.met810.models.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move
import org.bu.met810.{Turn, WinnerId, choose}


/**
  *  Save sample if and only if
  *  - sample does not yet exist in training set AND one of the following
  *  - if game ended playerModel to train was the winner
  *  - if game not needed move leads to state that is in training set
  */
object DataGenerator {

  def generateData(outputFilePath: String, boardSize: Int, numSamples: Int = 4000, shouldApplyNoise: Boolean = false): Unit ={
    val start = System.currentTimeMillis()
    val playerId = 0
    val positions = 0 until boardSize

    for(_ <- 1 to numSamples) {
      val rX = choose(positions.iterator)
      val rY = choose(positions.iterator)
      val cX = choose(positions.filter(_ != rX).iterator)
      val cY = choose(positions.filter(_ != rY).iterator)
      val p1Model = RandomMoveModel()
      val p2Model = RandomMoveModel()
      val board = Board(Robber((rX, rY)), Cop((cX, cY)), boardSize, boardSize, Seq.empty[Building])
      generateDataPoint(playerId, outputFilePath, board, p1Model, p2Model, shouldApplyNoise)
    }
    val end = System.currentTimeMillis()
    println(s"Data generation time: ${(end - start)/1000.0}s")
  }

  private def generateDataPoint(playerId: Int, outputFilePath: String, initialBoard: Board,
                                p1Model: PlayerModel[Board, Player, Move],
                                p2Model: PlayerModel[Board, Player, Move],
                                shouldApplyNoise: Boolean): Unit = {
    var data = List.empty[(Board, Move, Turn)]

    val sim = Simulator(initialBoard, p1Model, p2Model)
    var result: Option[(Board, Move, Board)] = None
    var prevTurn = if(sim.turn == 0) 1 else 0
    while(!sim.isGameOver){
      result = sim.runSimulator()
      if(result.nonEmpty) {
        val (prevBoard, move, _) = result.get
        data = data :+ (prevBoard, move, prevTurn)
      }
      prevTurn = if(sim.turn == 0) 1 else 0
    }
    val winnerId: WinnerId = sim.getWinner.get.id
    if(winnerId == playerId) {
      data.foreach { case (board, move, turn) =>
        if(shouldApplyNoise){
          applyNoise(board.p2.position, 1, 0.5).foreach{ case (_, pos) =>
            val newP2 = board.p2.copy(pos)
            val newBoard = board.copy(p2 = newP2)
            saveVectors(outputFilePath, newBoard.toVector, move.toVector, turn, winnerId)          }
        }
        else saveVectors(outputFilePath, board.toVector, move.toVector, turn, winnerId)
      }
    }
  }

  private def applyNoise(pos: (Int, Int), positionRadius: Int, minFactor: Double): List[(Double, (Int, Int))] = {
    val (x, y) = pos
    for{
      xDelta <- -positionRadius to positionRadius
      yDelta <- -positionRadius to positionRadius
    } yield (1.0/ Math.max(minFactor, Math.hypot(xDelta, yDelta)), (x + xDelta, y + yDelta))
  }.toList

  private def saveVectors(filePath: String, boardVec: Seq[Double], moveVec: Seq[Double], turn: Int, winnerId: WinnerId): Unit ={
    val pw = new FileWriter(new File(filePath), true)
    pw.append(s"${boardVec.mkString(",")},${moveVec.mkString(",")},$turn,$winnerId \n")
    pw.close()
  }
}
