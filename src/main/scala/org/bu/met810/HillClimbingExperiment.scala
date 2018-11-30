package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.data.{DataGenerator, Simulator}
import org.bu.met810.models.generative.BayesianPlayerModel
import org.bu.met810.models.inference.{GenerativeModelLearner, Learner}
import org.bu.met810.models.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object HillClimbingExperiment {

  val boardSize = 8

  def main(args: Array[String]): Unit = {
    var maxWins = 450
    val numPlayers = 2
    val playerIdToTrainFor = 0
    val trainingFile = s"training_data_$boardSize.csv"
    val learner: Learner = GenerativeModelLearner()
    val useGenerativeParams = learner.isInstanceOf[GenerativeModelLearner]
    val paramsFile = s"trainedModels/temp_model_${boardSize}by$boardSize.json"

    for(_ <- 1 to 1000) {
      DataGenerator.generateData(trainingFile, boardSize, numSamples = 1)
      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)
      val model: PlayerModel[Board, Player, Move] = BayesianPlayerModel(paramsFile, useGenerativeParams)
      val modelName = model.getClass.toString.split('.').toList.last
      val (numRobberWins, _) = runTest(model)
      if(numRobberWins > maxWins) {
        maxWins = numRobberWins
        println(maxWins)
        val pw = new PrintWriter(s"trainedModels/current_best_params_${modelName}_${boardSize}by$boardSize.json")
        val savedParams = scala.io.Source.fromFile(paramsFile).mkString
        pw.write(savedParams)
        pw.close()
      }
      val pw = new PrintWriter(trainingFile)
      pw.write("")
    }
  }

  private def runTest(model: PlayerModel[Board, Player, Move]): (Int, Int) = {
    val positions = 0 until boardSize
    val start = System.currentTimeMillis()
    val winners: Seq[Player] = for(_ <- 1 to 1000) yield {
      val rX = choose(positions.iterator)
      val rY = choose(positions.iterator)
      val cX = choose(positions.filter(_ != rX).iterator)
      val cY = choose(positions.filter(_ != rY).iterator)
      val board = Board(Robber((rX, rY)), Cop((cX, cY)), boardSize, boardSize, Seq.empty[Building])
      val sim = Simulator(board, model, RandomMoveModel())
      sim.runFullGame()
    }
    val robbers = winners.filter(p => p.isInstanceOf[Robber])
    val cops = winners.filter(p => p.isInstanceOf[Cop])
    val end = System.currentTimeMillis()
    println(s"runtime: ${(end - start)/1000}")
    (robbers.size, cops.size)
  }
}
