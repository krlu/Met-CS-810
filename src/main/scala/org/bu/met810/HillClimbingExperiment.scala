package org.bu.met810

import java.io.PrintWriter

import org.bu.met810.data.{BattleshipSim, DataGenerator, SimBuilder}
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.models.learners.{BayesianModelLearner, GenerativeModelLearner, Learner}
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.battleshipassets._
import org.bu.met810.types.{Agent, Environment, Vectorizable}


object HillClimbingExperiment {

  def main(args: Array[String]): Unit = {
    val P1_ID = 0
    val P2_ID = 1
    val boardSize = 5
    val moveDim = 2
    val agentDim = boardSize*boardSize*2

    def vectorToMove(vector: Seq[Int]): Move = Move(vector.head, vector(1))
    def vectorToBoard(vector: Seq[Int]): Board = {
      val posVectors = possiblePositions(boardSize,boardSize,2)
      def vectorToPlayer(playerVector: Seq[Int], id: Int): Player = {
        val (posStatuses, moveMadeStatus) = playerVector.splitAt(playerVector.size/2)
        val positions = (posVectors zip posStatuses).map{ case (pos, status) => (pos.head, pos(1)) -> status}
        val movesMade = (posVectors zip moveMadeStatus).map{ case (pos, status) => (pos.head, pos(1)) -> status}
        Player(positions, id, movesMade)
      }
      val playerDataVector = vector.dropRight(2)
      val dimensions = vector.takeRight(2)
      val (p1Vector, p2Vector) = playerDataVector.splitAt(playerDataVector.length/2)
      val p1 = vectorToPlayer(p1Vector, 0)
      val p2 = vectorToPlayer(p2Vector, 1)
      Board(p1, p2, dimensions.head, dimensions(1))
    }

    val possibleMoves = possiblePositions(boardSize, boardSize, moveDim).map{vectorToMove}
    val possiblePositionsStates = permutationsWithRepetitions(List(0,1), agentDim/2)
    val possibleMovesStates = permutationsWithRepetitions(List(-1, 0,1), agentDim/2)
    val possibleStates = for{
        positions <- possiblePositionsStates
        moves <- possibleMovesStates
      } yield {
      List(P1_ID) ++ positions ++ moves
    }
    runAllExperiments(possibleMoves, possibleMoves, vectorToBoard, vectorToMove,
      BattleshipSim, agentDim, Array(P1_ID, P2_ID), possibleStates, playerId = P1_ID, boardSize)
  }

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

    require(ids.length == 2)
    val iter1: (String, Boolean) => PlayerModel[Env, A, Action]= DeterministicPlayerModel.apply(_, _, p1Moves)
    val iter2: (String, Boolean) => PlayerModel[Env, A, Action] = BayesianPlayerModel.apply(_, _, p1Moves)
    val paramsFile = s"temp_model.json"
    for{
      iterateWithNoise <- List(false)
      trainingSize <- List(1000)
      learner <- List(
        new GenerativeModelLearner[Env, A, Action](vectorToBoard, vectorToMove, agentDim,
          (p1Moves ++ p2Moves).distinct, possibleStates),
        new BayesianModelLearner[Env, A, Action](vectorToBoard, vectorToMove, paramsFile,
          useGenerativeParams = true, agentDim, (p1Moves ++ p2Moves).distinct, possibleStates))
      iterationModelBuilder <- List(iter1, iter2)
    } {
      runOneExperiment(playerId, ids.length, learner, iterationModelBuilder, iterateWithNoise, trainingSize)
    }

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

      for(_ <- 1 to 300) {
        val pw = new PrintWriter(trainingFile)
        pw.write("")

        var start = System.currentTimeMillis()
        DataGenerator.generateData[Env, A, Action](
          trainingFile, boardSize, numTrainingSamples, playerIdToTrainFor, simBuilder,
          new RandomMoveModel[Env, A, Action](p1Moves), new RandomMoveModel[Env, A, Action](p2Moves))
        var end = System.currentTimeMillis()
        println(s"Data generation time: ${(end - start)/1000.0}s")

        start = System.currentTimeMillis()
        println("training player model...")
        learner.learn(trainingFile, boardSize, numPlayers, playerId = playerIdToTrainFor, paramsFile)
        end = System.currentTimeMillis()
        println(s"Training time: ${(end - start)/1000.0}s")

        val p1Model: PlayerModel[Env, A, Action] = iterationModelBuilder(paramsFile, useGenerativeParams)
        val p2Model: PlayerModel[Env, A, Action] = new RandomMoveModel[Env, A, Action](p2Moves)
        val modelName = p1Model.getClass.toString.split('.').toList.last
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
