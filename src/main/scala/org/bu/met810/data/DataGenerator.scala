package org.bu.met810.data

import java.io.{File, FileWriter}

import org.bu.met810.{Turn, WinnerId}
import org.bu.met810.model.RandomMoveModel
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move


/**
  *  Save sample if and only if
  *  - sample does not yet exist in training set AND one of the following
  *  - if game ended playerModel to train was the winner
  *  - if game not needed move leads to state that is in training set
  */
object DataGenerator {

  def main(args: Array[String]): Unit = {
    val playerId = 0
    val outputFilePath: String = "training_data.csv"
    val numRows = 4
    val numCols = 4
    for(i <- 1 to 2000) {
      generateDataPoint(playerId, outputFilePath, numRows, numCols)
      println(i)
    }
  }

  def generateDataPoint(playerId: Int, outputFilePath: String, numRows: Int, numCols: Int): Unit ={
    val initialBoard = Board(Robber((0, 0)), Cop((3, 3)), numRows, numCols, Seq.empty[Building])
    var data = List.empty[(Board, Move, Turn)]
    val sim = Simulator(initialBoard, RandomMoveModel(), RandomMoveModel())
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
    data.foreach{ case (board, move, turn) =>
      saveVectors(outputFilePath, board.toVector, move.toVector, turn, winnerId)
    }
  }

  private def saveVectors(filePath: String, boardVec: Seq[Double], moveVec: Seq[Double], turn: Int, winnerId: WinnerId): Unit ={
    val pw = new FileWriter(new File(filePath), true)
    pw.append(s"${boardVec.mkString(",")},${moveVec.mkString(",")},$turn,$winnerId \n")
    pw.close()
  }
}
