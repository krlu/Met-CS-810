package org.bu.simmba.models.learners

import org.bu.simmba.types.{Agent, Environment}
import org.bu.simmba.{Turn, WinnerId, getTrainingData}

trait Learner[Env <: Environment[Action, A], A <:Agent, Action]{

  def learn(trainingDataFilePath: String, boardSize: Int, numPlayers: Int, playerId: Int, paramsFile: String): Unit

  def getFeaturizedTrainingData(filePath: String, boardDim: Int = 6, moveDim: Int = 2):
  Seq[(Env, Action, Turn, WinnerId)] = {
    getTrainingData(filePath, boardDim, moveDim).map{ case (boardVec, moveVec, turn, winner) =>
      (vectorToBoard(boardVec), vectorToMove(moveVec), turn, winner)
    }
  }

  val vectorToBoard: Seq[Int] => Env
  val vectorToMove: Seq[Int] => Action
  val agentDim: Int
}
