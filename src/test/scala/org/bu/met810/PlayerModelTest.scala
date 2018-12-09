package org.bu.met810

import org.bu.met810.data.Simulator
import org.bu.met810.models.generative.{BayesianPlayerModel, DeterministicPlayerModel}
import org.scalatest.{FlatSpec, Matchers}

class PlayerModelTest extends FlatSpec with Matchers {


  "Random robber model" should "win infrequently" in {
    println(Simulator.runBatch())
  }

  "Bayesian robber model" should "win often with Bayesian model" in {
    val model = new BayesianPlayerModel("trainedModels/current_best_robber_BayesianPlayerModel_4by4.json", useGenerativeParams = true)
    println(Simulator.runBatch(model))
  }

  "Bayesian robber model" should "win often with Deterministic Model" in {
    val model = new DeterministicPlayerModel("trainedModels/current_best_params_DeterministicPlayerModel_4by4.json", useGenerativeParams = true)
    println(Simulator.runBatch(model, numTrials = 10000))
  }
}

