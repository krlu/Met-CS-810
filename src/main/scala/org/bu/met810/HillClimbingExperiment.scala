package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.data.{CopsAndRobbersSim, DataGenerator, Simulator}
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.models.learners.{GenerativeModelLearner, Learner}
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.copsandrobbersassets.{Move, _}

/**
  * Experiment for Cops and Robbers
  */
object HillClimbingExperiment {

  val ROBBER_ID = 0
  val COP_ID = 1

  def main(args: Array[String]): Unit = {
    val isValidState: Seq[Int] => Boolean = state  => state(1) != state(3) || state(2) != state(4)

    val iter1: (String, Boolean) => PlayerModel[Board, Player, Move]= DeterministicPlayerModel.apply(_, _, Move.robberMoves)
    val iter2: (String, Boolean) => PlayerModel[Board, Player, Move]= BayesianPlayerModel.apply(_, _, Move.robberMoves)
    val paramsFile = s"temp_model.json"
    val playerId = 0
    val numPlayers = 2
    val boardSize = 4
    for{
      iterateWithNoise <- List(false)
      trainingSize <- List(2,4,8)
      learner <- List(new GenerativeModelLearner[Board, Player, Move](vectorToBoard, vectorToMove, isValidState, Move.robberMoves))
      //,BayesianModelLearner(paramsFile, useGenerativeParams = false))
      iterationModelBuilder <- List(iter1, iter2)
    } run(playerId, numPlayers, boardSize, learner, iterationModelBuilder, iterateWithNoise, paramsFile, trainingSize)
  }

  def run(playerIdToTrainFor: Int, numPlayers: Int, boardSize: Int, learner: Learner[Board, Player, Move],
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
    val useGenerativeParams = learner.isInstanceOf[GenerativeModelLearner[Board, Player, Move]]

    for(_ <- 1 to 300) {

      val sim: Board => Simulator[Board, Player, Move] =
        CopsAndRobbersSim(_, RandomMoveModel.crModel(Move.robberMoves), RandomMoveModel.crModel(Move.copMoves), iterateWithNoise)

      var start = System.currentTimeMillis()
      DataGenerator.generateData[Board, Player, Move](trainingFile, boardSize, numTrainingSamples, numPlayers, playerIdToTrainFor, sim, enumerateAllCopsAndRobbersStates)
      var end = System.currentTimeMillis()
      println(s"Data generation time: ${(end - start)/1000.0}s")
      start = System.currentTimeMillis()
      println("training player model...")
      learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)
      end = System.currentTimeMillis()
      println(s"Training time: ${(end - start)/1000.0}s")

      val robberModel: PlayerModel[Board, Player, Move] = iterationModelBuilder(paramsFile, useGenerativeParams)
      val copModel: PlayerModel[Board, Player, Move] = RandomMoveModel.crModel(Move.copMoves)
      val modelName = robberModel.getClass.toString.split('.').toList.last
      val learnerName = learner.getClass.toString.split('.').toList.last

      start = System.currentTimeMillis()
      val winners = CopsAndRobbersSim.runBatch(robberModel, copModel, shouldApplyNoise = iterateWithNoise)
      end = System.currentTimeMillis()
      println(s"runtime: ${(end - start).toDouble/1000}")

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

  private def vectorToBoard(vector: Seq[Int]): Board = {
    val p1 = Robber((vector.head, vector(1)))
    val p2 = Cop((vector(2), vector(3)))
    Board(p1, p2, vector(4), vector(5), Seq())
  }

  private def vectorToMove(vector: Seq[Int]): Move =
    Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight).find(_.toVector == vector.map(_.ceil.toInt)) match {
      case Some(move) => move
      case None => throw new NoSuchElementException(s"unable to find move with vector ${vector.map(_.ceil.toInt)}!")
  }
}
