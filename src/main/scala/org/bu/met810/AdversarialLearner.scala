package org.bu.met810

import neuroflow.core.Activators.Double._
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._


//TODO: Current uses toy example from Neuroflow site
class AdversarialLearner {
  implicit val weights: WeightBreeder[Double] = WeightBreeder[Double].normal(μ = 0.0, σ = 0.1)

  val (g, h) = (Sigmoid, Sigmoid)

  val net = Network(
    layout = Vector(2) :: Dense(3, g) :: Dense(1, h) :: SquaredError()
  )

}
