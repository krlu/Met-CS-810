package org.bu.met810.data

import java.io.{File, FileWriter}

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
    for(_ <- 1 to 10000)
      generateDataPoint(playerId, outputFilePath)
  }

  def generateDataPoint(playerId: Int, outputFilePath: String): Unit ={
    val board = Board(Robber((0, 0)), Cop((9, 9)), 10, 10, Seq.empty[Building])
    var data = getTrainingStates(outputFilePath, board.toVector.size)
    val sim = Simulator(board, RandomMoveModel(), RandomMoveModel())
    var mostRecentResult: Option[(Board, Move)] = None
    var result: Option[(Board, Move, Board)] = None
    do{
      result = sim.runSimulator()
      result match{
        case None =>
        case Some((b, m, newB)) =>
          val (boardVector, moveVector, newBoardVector) = (b.toVector, m.toVector, newB.toVector)
          mostRecentResult = Some((b, m))
          if(data.contains(newBoardVector) && !data.contains(boardVector)) {
            saveVectors(outputFilePath, boardVector, moveVector)
            data = data :+ boardVector
            return
          }
      }
    } while(result.nonEmpty)
    println(sim.getWinner, mostRecentResult)
    (mostRecentResult, sim.getWinner) match{
      case (Some((b, m)), Some(w)) =>
        val (boardVector, moveVector) = (b.toVector, m.toVector)
        if(w.id == playerId && !data.contains(boardVector))
          saveVectors(outputFilePath, boardVector, moveVector)
      case _ =>
    }
  }

  private def saveVectors(filePath: String, boardVec: Seq[Double], moveVec: Seq[Double]): Unit ={
    val pw = new FileWriter(new File(filePath), true)
    pw.append(s"${boardVec.mkString(",")},${moveVec.mkString(",")}\n")
    pw.close()
  }

  private def getTrainingStates(filePath: String, boardVecSize: Int): Seq[Seq[Double]] ={
    val bufferedSource = scala.io.Source.fromFile(filePath)
    val lines = bufferedSource.getLines().toList
    val data = {
      for (line <- lines) yield {
        val cols = line.split(",").map(_.trim.toDouble)
        cols.take(boardVecSize).toList
      }
    }
    bufferedSource.close()
    data
  }
}
