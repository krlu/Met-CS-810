package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.data.{DataGenerator, Simulator}
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.generative.BayesianPlayerModel
import org.bu.met810.models.inference.{GenerativeModelLearner, Learner}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object HillClimbingExperiment {

  def main(args: Array[String]): Unit = {

    val ROBBER_ID = 0
    val COP_ID = 1

    var maxWins = 0
    val numPlayers = 2
    val playerIdToTrainFor = COP_ID
    val boardSize = 4
    val trainingFile = s"training_data_$boardSize.csv"
    val learner: Learner = GenerativeModelLearner()
    val useGenerativeParams = learner.isInstanceOf[GenerativeModelLearner]
    val paramsFile = s"trainedModels/temp_model_${boardSize}by$boardSize.json"
    val robberParamsFile = s"trainedModels/current_best_robber_BayesianPlayerModel_${boardSize}by$boardSize.json"
    val robberModel: PlayerModel[Board, Player, Move] = BayesianPlayerModel(robberParamsFile, useGenerativeParams)

    for(_ <- 1 to 1000) {
      DataGenerator.generateData(
        outputFilePath = trainingFile,
        boardSize = boardSize,
        numSamples = 10,
        playerId = playerIdToTrainFor,
        p1Model = robberModel
      )
      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)
      val copModel: PlayerModel[Board, Player, Move] = BayesianPlayerModel(paramsFile, useGenerativeParams)
      val modelName = robberModel.getClass.toString.split('.').toList.last
      val (numRobberWins, numCopWins) = Simulator.runBatch(robberModel, copModel)
      val totalWins = numRobberWins + numCopWins
      val wins = if(playerIdToTrainFor == ROBBER_ID) numRobberWins else numCopWins
      if(wins > maxWins) {
        maxWins = wins
        println(maxWins, totalWins, maxWins.toDouble/totalWins)
        val pw = new PrintWriter(s"trainedModels/${playerIdToTrainFor}_${modelName}_${boardSize}by$boardSize.json")
        val savedParams = scala.io.Source.fromFile(paramsFile).mkString
        pw.write(savedParams)
        pw.close()
      }
      val pw = new PrintWriter(trainingFile)
      pw.write("")
    }
  }
}
