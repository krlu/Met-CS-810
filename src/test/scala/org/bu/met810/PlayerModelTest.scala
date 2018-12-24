package org.bu.met810

import java.io.FileWriter

import org.bu.met810.data.CopsAndRobbersSim
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move
import org.scalatest.{FlatSpec, Matchers}

class PlayerModelTest extends FlatSpec with Matchers {

  "Random robber model" should "win infrequently" in {
    println(CopsAndRobbersSim.runBatch())
  }

  val generative = "Generative"
  val deterministic = "Deterministic"
  val bayesian = "Bayesian"
  "Bayesian robber model" should "win often with Bayesian model" in {
    val builder1: (String, Boolean, Boolean) => PlayerModel[Board, Player, Move]= DeterministicPlayerModel.apply
    val builder2: (String, Boolean, Boolean) => PlayerModel[Board, Player, Move]= BayesianPlayerModel.apply
    val fw = new FileWriter("results.csv", true)
    fw.write("learnerType,iteratorType,trainedWithNoise,testWithNoise,trainingSize,modelName,robberWins,copWins,winPct\n")
    for{
      modelBuilder <- List(builder1, builder2)
      learnerType <- List(generative, bayesian)
      iteratorType <- List(deterministic, bayesian)
      trainedWithNoise <- List(true)
      testWithNoise <- List(true)
      trainingSize <- List(2,4)
    }{
      val paramsFile = s"trainedModels/${learnerType}ModelLearner_${iteratorType}PlayerModel_${trainedWithNoise}_$trainingSize.json"
      if(paramsFile != "trainedModels/BayesianModelLearner_BayesianPlayerModel_true_4.json") {
        val model = modelBuilder(paramsFile, learnerType == generative, testWithNoise)
        val modelName = model.getClass.toString.split('.').toList.last
        val trials = if (model.isInstanceOf[DeterministicPlayerModel]) 10000 else 1000
        val (robberWins, copWins) = CopsAndRobbersSim.runBatch(model, numTrials = trials)
        val winPct = robberWins.toDouble / (robberWins + copWins)
        fw.write(List(learnerType, iteratorType, trainedWithNoise, testWithNoise,
          trainingSize, modelName, robberWins, copWins, winPct
        ).mkString(",") + "\n"
        )
      }
    }
    fw.close()
  }

}

