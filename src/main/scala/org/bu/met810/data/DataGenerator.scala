package org.bu.met810.data

import java.io.{File, FileWriter}

import org.bu.met810.model.bayes.BayesianPlayerModel
import org.bu.met810.model.{PlayerModel, RandomMoveModel}
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

  def generateData(outputFilePath: String, boardSize: Int): Unit ={
    val playerId = 0
    val numRows = boardSize
    val numCols = boardSize
    var numRobberWins = 0
    val positions = 0 until boardSize
    for{_ <- 1 to 2000} {
      val rX = choose(positions.iterator)
      val rY = choose(positions.iterator)
      val cX = choose(positions.filter(_ != rX).iterator)
      val cY = choose(positions.filter(_ != rY).iterator)
      val p1Model = RandomMoveModel()
      val p2Model = RandomMoveModel()
      val initialBoard = Board(Robber((rX, rY)), Cop((cX, cY)), numRows, numCols, Seq.empty[Building])
      val winner = generateDataPoint(playerId, outputFilePath, initialBoard, p1Model, p2Model)
      if(winner == 0) {
        numRobberWins += 1
//        println(numRobberWins)
      }
    }
  }

  private def generateDataPoint(playerId: Int, outputFilePath: String, initialBoard: Board,
                        p1Model: PlayerModel[Board, Player, Move],
                        p2Model: PlayerModel[Board, Player, Move]): WinnerId ={
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
    if(winnerId == 0) {
      data.foreach { case (board, move, turn) =>
        saveVectors(outputFilePath, board.toVector, move.toVector, turn, winnerId)
      }
    }
    winnerId
  }

  private def saveVectors(filePath: String, boardVec: Seq[Double], moveVec: Seq[Double], turn: Int, winnerId: WinnerId): Unit ={
    val pw = new FileWriter(new File(filePath), true)
    pw.append(s"${boardVec.mkString(",")},${moveVec.mkString(",")},$turn,$winnerId \n")
    pw.close()
  }
}
