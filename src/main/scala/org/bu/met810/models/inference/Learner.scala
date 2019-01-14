package org.bu.met810.models.inference

import neuroflow.application.plugin.Notation.->
import org.bu.met810.types.copsandrobbersassets._
import org.bu.met810.{NNVector, Turn, WinnerId}

import scala.io.Source

trait Learner {

  def learn(trainingDataFilePath: String, boardSize: Int, numPlayers: Int, playerId: Int, paramsFile: String): Unit

  def getNNTrainingData(filePath: String, boardDim: Int = 6, moveDim: Int = 2):
  List[(NNVector, NNVector, Turn, WinnerId)] =
    getTrainingData(filePath, boardDim, moveDim).map{ case (boardVec, moveVec, turn, winner) =>
    (->(boardVec.map(_.toDouble):_*), ->(moveVec.map(_.toDouble):_*), turn, winner)
  }

  def getFeaturizedTrainingData(filePath: String, boardDim: Int = 6, moveDim: Int = 2):
  Seq[(Board, Move, Turn, WinnerId)] = {
    getTrainingData(filePath, boardDim, moveDim).map{ case (boardVec, moveVec, turn, winner) =>
      (vectorToBoard(boardVec), vectorToMove(moveVec), turn, winner)
    }
  }

  def getTrainingData(filePath: String, boardDim: Int, moveDim: Int): List[(Seq[Int], Seq[Int], Turn, WinnerId)] = {
    val bufferedSource = Source.fromFile(filePath)
    val data = {for (line <- bufferedSource.getLines) yield {
      val cols = line.split(",").map(_.trim.toDouble.toInt).toList
      val input = cols.take(boardDim)
      val output = cols.slice(boardDim, boardDim + moveDim)
      val turn = cols(cols.size - 2)
      val winner = cols.last
      (input, output, turn, winner)
    }}.toList
    bufferedSource.close
    data
  }

  private def vectorToBoard(vector: Seq[Int]): Board = {
    val p1 = Robber((vector.head, vector(1)))
    val p2 = Cop((vector(2), vector(3)))
    Board(p1, p2, vector(4), vector(5), Seq())
  }
  private def vectorToMove(vector: Seq[Int]): Move =
    Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight).find(_.toVector == vector.map(_.ceil.toInt)) match {
      case Some(move) => move
      case None => throw new NoSuchElementException(s"unable to find move with vector ${vector.map(_.ceil.toInt)}!")
    }
}
