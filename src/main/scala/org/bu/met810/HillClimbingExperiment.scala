package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.data.{CopsAndRobbersSim, DataGenerator, Simulator}
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.models.inference.{BayesianModelLearner, GenerativeModelLearner, Learner}
import org.bu.met810.models.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object HillClimbingExperiment {

  val ROBBER_ID = 0
  val COP_ID = 1

  def main(args: Array[String]): Unit = {
    val iter1: (String, Boolean, Boolean) => PlayerModel[Board, Player, Move]= DeterministicPlayerModel.apply
    val iter2: (String, Boolean, Boolean) => PlayerModel[Board, Player, Move]= BayesianPlayerModel.apply
    val paramsFile = s"trainedModels/temp_model.json"
    val playerId = 0
    val numPlayers = 2
    val boardSize = 4
    for{
      iterateWithNoise <- List(true, false)
      trainingSize <- List(2,4,8)
      learner <- List(GenerativeModelLearner(iterateWithNoise),
        BayesianModelLearner(paramsFile, useGenerativeParams = false, iterateWithNoise))
      iterationModelBuilder <- List(iter1, iter2)
    } run(playerId, numPlayers, boardSize, learner, iterationModelBuilder, iterateWithNoise, paramsFile, trainingSize)
  }

  def run(playerIdToTrainFor: Int, numPlayers: Int, boardSize: Int, learner: Learner,
          iterationModelBuilder: (String, Boolean, Boolean) => PlayerModel[Board, Player, Move],
          iterateWithNoise: Boolean, paramsFile: String, numTrainingSamples: Int): Unit ={

    def enumerateAllCopsAndRobbersStates(numRows: Int, numCols: Int, numPlayers: Int): List[Board] = {
      val pos = possiblePositions(numRows, numRows)
      pos.combinations(numPlayers).toList.map{ players =>
        Board(Robber(players.head), Cop(players(1)), boardSize, boardSize, Seq.empty[Building])
      }
    }

    var maxWins = 0
    val trainingFile = s"training_data_$boardSize.csv"
    val useGenerativeParams = learner.isInstanceOf[GenerativeModelLearner]

    for(_ <- 1 to 300) {

      val sim: Board => Simulator[Board, Player, Move] =
        CopsAndRobbersSim(_, RandomMoveModel(), RandomMoveModel(), iterateWithNoise)
      DataGenerator.generateData[Board, Player, Move](trainingFile, boardSize,
        numTrainingSamples, numPlayers, playerIdToTrainFor, sim, enumerateAllCopsAndRobbersStates)
      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)

      val robberModel: PlayerModel[Board, Player, Move] = iterationModelBuilder(paramsFile, useGenerativeParams, false)
      val copModel: PlayerModel[Board, Player, Move] = RandomMoveModel()
      val modelName = robberModel.getClass.toString.split('.').toList.last
      val learnerName = learner.getClass.toString.split('.').toList.last
      val (numRobberWins, numCopWins) = CopsAndRobbersSim.runBatch(robberModel, copModel, shouldApplyNoise = iterateWithNoise)
      val totalWins = numRobberWins + numCopWins
      val wins = if(playerIdToTrainFor == ROBBER_ID) numRobberWins else numCopWins
      if(wins > maxWins) {
        maxWins = wins
        println(maxWins, totalWins, maxWins.toDouble/totalWins)
        val pw = new PrintWriter(s"trainedModels/${learnerName}_${modelName}_${iterateWithNoise}_$numTrainingSamples.json")
        val savedParams = scala.io.Source.fromFile(paramsFile).mkString
        pw.write(savedParams)
        pw.close()
      }
      val pw = new PrintWriter(trainingFile)
      pw.write("")
    }
  }
}
