package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.data.{DataGenerator, Simulator}
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.models.inference.{BayesianModelLearner, GenerativeModelLearner, Learner}
import org.bu.met810.models.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object HillClimbingExperiment {

  val ROBBER_ID = 0
  val COP_ID = 1

  def main(args: Array[String]): Unit = {
    val iter1: (String, Boolean) => PlayerModel[Board, Player, Move]= DeterministicPlayerModel.apply
    val iter2: (String, Boolean) => PlayerModel[Board, Player, Move]= BayesianPlayerModel.apply
    for{
      iterateWithNoise <- List(false, true)
      trainingSize <- List(2,4,8,16)
      iterationModelBuilder <- List(iter1, iter2)
      playerId = 0
      numPlayers = 2
      boardSize = 4
      paramsFile = s"trainedModels/temp_model_${boardSize}by$boardSize.json"
      learner <- List(GenerativeModelLearner(), BayesianModelLearner(paramsFile, useGenerativeParams = false))
    } run(playerId, numPlayers, boardSize, learner, iterationModelBuilder, iterateWithNoise, paramsFile, trainingSize)
  }

  def run(playerIdToTrainFor: Int, numPlayers: Int, boardSize: Int, learner: Learner,
          iterationModelBuilder: (String, Boolean) => PlayerModel[Board, Player, Move],
          iterateWithNoise: Boolean, paramsFile: String, numTrainingSamples: Int): Unit ={
    var maxWins = 0
    val trainingFile = s"training_data_$boardSize.csv"
    val useGenerativeParams = learner.isInstanceOf[GenerativeModelLearner]

    for(_ <- 1 to 100) {

      DataGenerator.generateData(trainingFile, boardSize, numTrainingSamples, iterateWithNoise, numPlayers, playerIdToTrainFor)
      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)

      val robberModel: PlayerModel[Board, Player, Move] = iterationModelBuilder(paramsFile, useGenerativeParams)
      val copModel: PlayerModel[Board, Player, Move] = RandomMoveModel()
      val modelName = robberModel.getClass.toString.split('.').toList.last
      val learnerName = learner.getClass.toString.split('.').toList.last
      val (numRobberWins, numCopWins) = Simulator.runBatch(robberModel, copModel)
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
