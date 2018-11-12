package org.bu.met810

import java.io.PrintWriter
import argonaut.Argonaut._

import org.bu.met810.data.{DataGenerator, Simulator}
import org.bu.met810.model.{PlayerModel, RandomMoveModel}
import org.bu.met810.model.bayes.{BayesianPlayerModel, PlayerModelLearner}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object HillClimbingExperiment {
  def main(args: Array[String]): Unit = {
    var maxWins = 0
    val trainingFile = "training_data.csv"
    for(_ <- 1 to 1000) {
      DataGenerator.generateData(trainingFile)
      PlayerModelLearner.learn(trainingFile)
      val model = new BayesianPlayerModel("model_0_4by4.json")
      val (numRobberWins, _) = runTest(model)
      if(numRobberWins > maxWins) {
        maxWins = numRobberWins
        println(maxWins)
        val pw = new PrintWriter("current_best_params.json")
        val savedParams = model.modelParams.asJson.toString()
        pw.write(savedParams)
        pw.close()
      }
      val pw = new PrintWriter(trainingFile)
      pw.write("")
    }
  }



  private def runTest(model: PlayerModel[Board, Player, Move]): (Int, Int) = {
    val start = System.currentTimeMillis()
    val winners: Seq[Player] = for(i <- 1 to 1000) yield {
      val rX = choose(List(0,1,2,3).iterator)
      val rY = choose(List(0,1,2,3).iterator)
      val cX = choose(List(0,1,2,3).filter(_ != rX).iterator)
      val cY = choose(List(0,1,2,3).filter(_ != rY).iterator)
      val board = Board(Robber((rX, rY)), Cop((cX, cY)), 4, 4, Seq.empty[Building])
      val sim = Simulator(board, model, RandomMoveModel())
//      println(i)
      sim.runFullGame()
    }
    val robbers = winners.filter(p => p.isInstanceOf[Robber])
    val cops = winners.filter(p => p.isInstanceOf[Cop])
    val end = System.currentTimeMillis()
    println(s"runtime: ${(end - start)/1000}")
    (robbers.size, cops.size)
  }
}
