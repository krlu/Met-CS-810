package org.bu.met810

import java.io.PrintWriter

import argonaut.Argonaut._
import org.bu.met810.data.{DataGenerator, Simulator}
import org.bu.met810.model.bayes.{BayesianPlayerModel, PlayerModelLearner}
import org.bu.met810.model.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object HillClimbingExperiment {

  val boardSize = 8

  def main(args: Array[String]): Unit = {
    var maxWins = 0
    val trainingFile = s"training_data_$boardSize.csv"
    for(_ <- 1 to 1000) {
      DataGenerator.generateData(trainingFile, boardSize)
      PlayerModelLearner.learn(trainingFile, boardSize, playerId = 0)
      val model = new BayesianPlayerModel(s"model_0_${boardSize}by$boardSize.json")
      val (numRobberWins, _) = runTest(model)
      if(numRobberWins > maxWins) {
        maxWins = numRobberWins
        println(maxWins)
        val pw = new PrintWriter(s"current_best_params_${boardSize}by$boardSize.json")
        val savedParams = model.modelParams.asJson.toString()
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
    val winners: Seq[Player] = for(i <- 1 to 1000) yield {
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
