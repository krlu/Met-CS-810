package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.data.{CopsAndRobbersSim, DataGenerator, Simulator}
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.models.learners.{GenerativeModelLearner, Learner}
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.copsandrobbersassets.{Move, _}

object HillClimbingExperiment {

  val ROBBER_ID = 0
  val COP_ID = 1

  def main(args: Array[String]): Unit = {
    val iter1: (String, Boolean) => PlayerModel[Board, Player, Move]= DeterministicPlayerModel.apply
    val iter2: (String, Boolean) => PlayerModel[Board, Player, Move]= BayesianPlayerModel.apply
    val paramsFile = s"temp_model.json"
    val playerId = 0
    val numPlayers = 2
    val boardSize = 4
    for{
      iterateWithNoise <- List(false)
      trainingSize <- List(8)
      learner <- List(GenerativeModelLearner())
      //,BayesianModelLearner(paramsFile, useGenerativeParams = false))
      iterationModelBuilder <- List(iter1, iter2)
    } run(playerId, numPlayers, boardSize, learner, iterationModelBuilder, iterateWithNoise, paramsFile, trainingSize)
  }

  def run(playerIdToTrainFor: Int, numPlayers: Int, boardSize: Int, learner: Learner,
          iterationModelBuilder: (String, Boolean) => PlayerModel[Board, Player, Move],
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
    val copMoves =  List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
    val robberMoves = List(Up, Down, Left, Right)

    for(_ <- 1 to 300) {

      val sim: Board => Simulator[Board, Player, Move] =
        CopsAndRobbersSim(_, RandomMoveModel.crModel(robberMoves), RandomMoveModel.crModel(copMoves), iterateWithNoise)
      DataGenerator.generateData[Board, Player, Move](trainingFile, boardSize, numTrainingSamples, numPlayers, playerIdToTrainFor, sim, enumerateAllCopsAndRobbersStates)
      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)

      val robberModel: PlayerModel[Board, Player, Move] = iterationModelBuilder(paramsFile, useGenerativeParams)
      val copModel: PlayerModel[Board, Player, Move] = RandomMoveModel.crModel(copMoves)
      val modelName = robberModel.getClass.toString.split('.').toList.last
      val learnerName = learner.getClass.toString.split('.').toList.last
      val winners = CopsAndRobbersSim.runBatch(robberModel, copModel, shouldApplyNoise = iterateWithNoise)
      val numRobberWins = winners.count(_.id == 0)
      val numCopWins =  winners.count(_.id == 1)
      val totalWins = winners.size
      val wins = if(playerIdToTrainFor == ROBBER_ID) numRobberWins else numCopWins
      if(wins > maxWins) {
        maxWins = wins
        println(maxWins, totalWins, maxWins.toDouble/totalWins)
        val pw = new PrintWriter(s"${learnerName}_${modelName}_${iterateWithNoise}_$numTrainingSamples.json")
        val savedParams = scala.io.Source.fromFile(paramsFile).mkString
        pw.write(savedParams)
        pw.close()
      }
      val pw = new PrintWriter(trainingFile)
      pw.write("")
    }
  }
}
