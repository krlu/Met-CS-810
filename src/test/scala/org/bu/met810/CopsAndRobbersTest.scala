package org.bu.met810

import java.io.FileWriter

import org.bu.met810.HillClimbingExperiment.runAllExperiments
import org.bu.met810.data.CopsAndRobbersSim
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.copsandrobbersassets._
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

  "Cops and robbers model" should "successfully train" in {
    val ROBBER_ID = 0
    val COP_ID = 1
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
    runAllExperiments(Move.robberMoves, Move.copMoves, vectorToBoard, vectorToMove,
      CopsAndRobbersSim, agentDim = 2, Array(ROBBER_ID, COP_ID), isValidState)
  }

  "Bayesian robber model" should "win often with Bayesian model" in {
    val builder1: (String, Boolean) => PlayerModel[Board, Player, Move]= DeterministicPlayerModel.apply(_, _, Move.possibleMoves)
    val builder2: (String, Boolean) => PlayerModel[Board, Player, Move]= BayesianPlayerModel.apply(_, _, Move.possibleMoves)
    val fw = new FileWriter("results.csv", true)
    fw.write("learnerType,iteratorType,trainedWithNoise,testWithNoise,trainingSize,modelName,robberWins,copWins,winPct\n")
    for{
      testWithNoise <- List(false)
      trainedWithNoise <- List(false)
      learnerType <- List(generative)
      iteratorType <- List(deterministic)
      trainingSize <- List(1000)
    }{
      val paramsFile = s"${learnerType}ModelLearner_${iteratorType}PlayerModel_${trainedWithNoise}_$trainingSize.json"
      val model =
        if(iteratorType == deterministic) builder1(paramsFile, learnerType == generative)
        else builder2(paramsFile, learnerType == generative)

      val modelName = model.getClass.toString.split('.').toList.last
      val trials = if (model.isInstanceOf[DeterministicPlayerModel[Board, Player, Move]]) 10000 else 1000
      val winners: Seq[Player] = CopsAndRobbersSim.runBatch(model, RandomMoveModel.crModel(Move.copMoves), numTrials = trials, shouldApplyNoise = testWithNoise)
      val robberWins = winners.count(_.id == 0)
      val copWins =  winners.count(_.id == 1)
      val winPct = robberWins.toDouble / (robberWins + copWins)
      fw.write(List(learnerType, iteratorType, trainedWithNoise, testWithNoise,
        trainingSize, modelName, robberWins, copWins, winPct
      ).mkString(",") + "\n"
      )
    }
    fw.close()
  }

}

