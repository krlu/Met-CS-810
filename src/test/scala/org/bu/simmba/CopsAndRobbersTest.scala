package org.bu.simmba

import java.io.FileWriter

import org.bu.simmba.HillClimbingExperiment.runAllExperiments
import org.bu.simmba.simulation.CopsAndRobbersSim
import org.bu.simmba.models.PlayerModel
import org.bu.simmba.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.simmba.models.random.RandomMoveModel
import org.bu.simmba.types.copsandrobbersassets._
import org.scalatest.{FlatSpec, Matchers}

class CopsAndRobbersTest extends FlatSpec with Matchers {

  "Random robber model" should "win infrequently" in {
    List(true, false).foreach { noise =>
      val winners = CopsAndRobbersSim.runBatch(
        RandomMoveModel.crModel(Move.robberMoves),
        RandomMoveModel.crModel(Move.copMoves),
        shouldApplyNoise = noise)

      val robberWins = winners.count(_.id == 0)
      val copWins =  winners.count(_.id == 1)
      println(robberWins, copWins)
    }
  }

  val generative = "Generative"
  val deterministic = "Deterministic"
  val bayesian = "Bayesian"
  val ROBBER_ID = 0
  val COP_ID = 1
  val boardSize = 4
  val agentDim = 2
  val numPlayers = 2
  val idForPlayer: Int = ROBBER_ID

  "Cops and robbers model" should "successfully train" in {

    val isValidState: Seq[Int] => Boolean = state  => state(1) != state(3) || state(2) != state(4)

    def vectorToBoard(vector: Seq[Int]): Board = {
      val p1 = Robber((vector.head, vector(1)))
      val p2 = Cop((vector(2), vector(3)))
      Board(p1, p2, vector(4), vector(5), Seq())
    }
    def vectorToMove(vector: Seq[Int]): Move =
      Move.possibleMoves.find(_.toVector == vector.map(_.ceil.toInt)) match {
        case Some(move) => move
        case None => throw new NoSuchElementException(s"unable to find move with vector ${vector.map(_.ceil.toInt)}!")
      }
    val possibleStates =
      permutationsWithRepetitions((0 until boardSize).toList, agentDim * numPlayers)
        .map{ state => List(idForPlayer) ++ state}
        .filter(isValidState)

    runAllExperiments(Move.robberMoves, Move.copMoves, vectorToBoard, vectorToMove,
      CopsAndRobbersSim, agentDim, Array(ROBBER_ID, COP_ID), possibleStates, idForPlayer, boardSize)
  }

  "Bayesian robber model" should "win often with Bayesian model" in {
    val builder1: (String, Boolean) => PlayerModel[Board, Player, Move]= DeterministicPlayerModel.apply(_, _, Move.robberMoves)
    val builder2: (String, Boolean) => PlayerModel[Board, Player, Move]= BayesianPlayerModel.apply(_, _, Move.robberMoves)
    val fw = new FileWriter("results.csv", true)
    fw.write("learnerType,iteratorType,trainedWithNoise,testWithNoise,trainingSize,modelName,robberWins,copWins,winPct\n")
    for{
      testWithNoise <- List(false)
      trainedWithNoise <- List(false)
      learnerType <- List(generative, bayesian)
      iteratorType <- List(deterministic, bayesian)
      trainingSize <- List(1000)
    }{
      val paramsFile = s"trainedModels/${learnerType}ModelLearner_${iteratorType}PlayerModel_${trainedWithNoise}_$trainingSize.json"
      val model =
        if(iteratorType == deterministic) builder1(paramsFile, learnerType == generative)
        else builder2(paramsFile, learnerType == generative)

      val modelName = model.getClass.toString.split('.').toList.last
      val trials = 10000
      val winners: Seq[Player] =
        if(idForPlayer == ROBBER_ID)
          CopsAndRobbersSim.runBatch(model, RandomMoveModel.crModel(Move.copMoves), numTrials = trials, shouldApplyNoise = testWithNoise)
        else if(idForPlayer == COP_ID)
          CopsAndRobbersSim.runBatch(RandomMoveModel.crModel(Move.robberMoves), model, numTrials = trials, shouldApplyNoise = testWithNoise)
        else throw new IllegalStateException(s"id for player should be $ROBBER_ID or $COP_ID but was $idForPlayer")
      val robberWins = winners.count(_.id == 0)
      val copWins =  winners.count(_.id == 1)
      val winPct = robberWins.toDouble / (robberWins + copWins)
      fw.write(List(learnerType, iteratorType, trainedWithNoise, testWithNoise,
        trainingSize, modelName, robberWins, copWins, winPct
      ).mkString(",") + "\n"
      )
      println(paramsFile)
    }
    fw.close()
  }

}

