package org.bu.met810.models.learners

import neuroflow.application.plugin.Notation.->
import org.bu.met810.types.{Agent, Environment}
import org.bu.met810.{NNVector, Turn, WinnerId}

import scala.io.Source

trait Learner[Env <: Environment[Action, A], A <:Agent, Action]{

  def learn(trainingDataFilePath: String, boardSize: Int, numPlayers: Int, playerId: Int, paramsFile: String): Unit

  def getNNTrainingData(filePath: String, boardDim: Int = 6, moveDim: Int = 2):
  List[(NNVector, NNVector, Turn, WinnerId)] =
    getTrainingData(filePath, boardDim, moveDim).map{ case (boardVec, moveVec, turn, winner) =>
    (->(boardVec.map(_.toDouble):_*), ->(moveVec.map(_.toDouble):_*), turn, winner)
  }

  def getFeaturizedTrainingData(filePath: String, boardDim: Int = 6, moveDim: Int = 2):
  Seq[(Env, Action, Turn, WinnerId)] = {
    getTrainingData(filePath, boardDim, moveDim).map{ case (boardVec, moveVec, turn, winner) =>
      (vectorToBoard(boardVec), vectorToMove(moveVec), turn, winner)
    }
  }

  private def getTrainingData(filePath: String, boardDim: Int, moveDim: Int): List[(Seq[Int], Seq[Int], Turn, WinnerId)] = {
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

  val vectorToBoard: Seq[Int] => Env
  val vectorToMove: Seq[Int] => Action
}
