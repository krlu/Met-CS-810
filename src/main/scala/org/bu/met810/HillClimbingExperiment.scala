package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.simulation.{DataGenerator, SimBuilder}
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.models.learners.{BayesianModelLearner, GenerativeModelLearner, Learner}
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.{Agent, Environment, Vectorizable}


object HillClimbingExperiment {

  def runAllExperiments[Env <: Environment[Action, A] with Vectorizable, A <: Agent with Vectorizable, Action <: Vectorizable](
    p1Moves: List[Action],
    p2Moves: List[Action],
    vectorToBoard: Seq[Int] => Env,
    vectorToMove: Seq[Int] => Action,
    simBuilder: SimBuilder[Env, A, Action],
    agentDim: Int, ids: Array[Int],
    possibleStates: Seq[Seq[Int]],
    playerId: Int,
    boardSize: Int): Unit = {
    val possibleMoves = if(playerId == ids(0)) p1Moves else p2Moves
    require(ids.length == 2)
    val iter1: (String, Boolean) => PlayerModel[Env, A, Action]= DeterministicPlayerModel.apply(_, _, possibleMoves)
    val iter2: (String, Boolean) => PlayerModel[Env, A, Action] = BayesianPlayerModel.apply(_, _, possibleMoves)
    val paramsFile = s"temp_model.json"
    val learner1 = new GenerativeModelLearner[Env, A, Action](vectorToBoard,
      vectorToMove, agentDim, possibleMoves, possibleStates)
    val learner2 = new BayesianModelLearner[Env, A, Action](vectorToBoard, vectorToMove,
      paramsFile, agentDim, possibleMoves, possibleStates)
    for{
      iterateWithNoise <- List(false)
      trainingSize <- List(1000)
      learner <- List(learner1, learner2)
      iterationModelBuilder <- List(iter1, iter2)
    } runOneExperiment(playerId, ids.length, learner, iterationModelBuilder, iterateWithNoise, trainingSize)

    def runOneExperiment(playerIdToTrainFor: Int,
                         numPlayers: Int,
                         learner: Learner[Env, A, Action],
                         iterationModelBuilder: (String, Boolean) => PlayerModel[Env, A, Action],
                         iterateWithNoise: Boolean,
                         numTrainingSamples: Int): Unit = {
      require(numPlayers == 2)
      var maxWins = 0
      val trainingFile = s"training_data_$boardSize.csv"
      val useGenerativeParams = learner.isInstanceOf[GenerativeModelLearner[Env, A, Action]]

      def whichModel(playerId: Int): (PlayerModel[Env, A, Action], PlayerModel[Env, A, Action]) = {
        if(playerId == ids(0))
          (iterationModelBuilder(paramsFile, useGenerativeParams), new RandomMoveModel[Env, A, Action](p2Moves))
        else
          (new RandomMoveModel[Env, A, Action](p1Moves), iterationModelBuilder(paramsFile, useGenerativeParams))
      }

      for(_ <- 1 to 50) {
        val pw = new PrintWriter(trainingFile)
        pw.write("")

        var start = System.currentTimeMillis()
        DataGenerator.generateData[Env, A, Action](
          trainingFile, boardSize, numTrainingSamples, playerIdToTrainFor, simBuilder,
          new RandomMoveModel[Env, A, Action](p1Moves),
          new RandomMoveModel[Env, A, Action](p2Moves))
        var end = System.currentTimeMillis()
        println(s"Data generation time: ${(end - start)/1000.0}s")

        start = System.currentTimeMillis()
        println("training player model...")
        learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)
        end = System.currentTimeMillis()
        println(s"Training time: ${(end - start)/1000.0}s")

        val (p1Model, p2Model) = whichModel(playerId)
        val modelName =
          if(playerId == ids(0)) p1Model.getClass.toString.split('.').toList.last
          else if(playerId == ids(1)) p2Model.getClass.toString.split('.').toList.last
          else throw new IllegalArgumentException(s"playerId should be one of ${ids.mkString(",")} but was $playerId")
        val learnerName = learner.getClass.toString.split('.').toList.last

        start = System.currentTimeMillis()
        val winners = simBuilder.runBatch(p1Model, p2Model, shouldApplyNoise = iterateWithNoise)
        end = System.currentTimeMillis()
        println(s"runtime: ${(end - start).toDouble/1000}")

        val numP1Wins = winners.count(_.id == ids(0))
        val numP2Wins =  winners.count(_.id == ids(1))
        val totalWins = winners.size
        val wins = if(playerIdToTrainFor == ids(0)) numP1Wins else numP2Wins
        if(wins > maxWins) {
          maxWins = wins
          println(maxWins, totalWins, maxWins.toDouble/totalWins)
          val pw = new PrintWriter(s"${learnerName}_${modelName}_${iterateWithNoise}_$numTrainingSamples.json")
          val savedParams = scala.io.Source.fromFile(paramsFile).mkString
          pw.write(savedParams)
          pw.close()
        }
      }
    }
  }


}
