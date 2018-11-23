package org.bu.met810

import org.bu.met810.data.Simulator
import org.bu.met810.models.inference.{Learner, NNLearner}
import org.bu.met810.models.neuralnets.NNModel
import org.bu.met810.models.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object HillClimbingExperiment {

  val boardSize = 4

  def main(args: Array[String]): Unit = {

    var maxWins = 0
    val numPlayers = 2
    val playerIdToTrainFor = 0
    val trainingFile = s"training_data_$boardSize.csv"
    val learner: Learner = NNLearner()

    for(_ <- 1 to 1) {
//      DataGenerator.generateData(trainingFile, boardSize, numSamples = 20000)
//      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor)
      val model = new NNModel(savedWeights = Some("savedWeights"))
      val (numRobberWins, _) = runTest(model)
//      println(numRobberWins)
//      if(numRobberWins > maxWins) {
//        maxWins = numRobberWins
//        println(maxWins)
//        val pw = new PrintWriter(s"current_best_params2_${boardSize}by$boardSize.json")
//        val savedParams = model.modelParams.asJson.toString()
//        pw.write(savedParams)
//        pw.close()
//      }
//      val pw = new PrintWriter(trainingFile)
//      pw.write("")
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
