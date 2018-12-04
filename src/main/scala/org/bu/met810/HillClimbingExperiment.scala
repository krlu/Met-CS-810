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
    var maxWins = 0
    val numPlayers = 2
    val playerIdToTrainFor = 0
    val boardSize = 4
    val trainingFile = s"training_data_$boardSize.csv"
    val learner: Learner = GenerativeModelLearner()
    val useGenerativeParams = learner.isInstanceOf[GenerativeModelLearner]
    val paramsFile = s"trainedModels/temp_model_${boardSize}by$boardSize.json"

    for(_ <- 1 to 1000) {
      DataGenerator.generateData(trainingFile, boardSize, numSamples = 34)
      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)
      val model: PlayerModel[Board, Player, Move] = BayesianPlayerModel(paramsFile, useGenerativeParams)
      val modelName = model.getClass.toString.split('.').toList.last
      val (numRobberWins, _) = Simulator.runBatch(model)
      if(numRobberWins > maxWins) {
        maxWins = numRobberWins
        println(maxWins)
        val pw = new PrintWriter(s"trainedModels/current_best_params_2${modelName}_${boardSize}by$boardSize.json")
        val savedParams = scala.io.Source.fromFile(paramsFile).mkString
        pw.write(savedParams)
        pw.close()
      }
      val pw = new PrintWriter(trainingFile)
      pw.write("")
    }
  }
}
