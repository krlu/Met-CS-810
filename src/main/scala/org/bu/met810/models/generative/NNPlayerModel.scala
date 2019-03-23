package org.bu.met810.models.generative

import neuroflow.application.plugin.Notation.->
import neuroflow.application.plugin.IO.{File, _}
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment}

class NNPlayerModel[Env <: Environment[Action, A], A <: Agent, Action](
                                          inputDim: Int,
                                          outputDim: Int,
                                          val paramsFile: Option[String],
                                          val vectorToMove: Seq[Double] => Action) extends PlayerModel[Env, A, Action]{
  //  private val f1 = Activators.Double.Sigmoid
  private val f2 = Activators.Double.Linear

  implicit val weights: WeightBreeder[Double] = paramsFile match {
    case None => WeightBreeder[Double].normal(μ = 0.0, σ = 2.0)
    case Some(filePath) => File.weightBreeder(filePath)
  }
  private val net = Network(
    layout = Vector (inputDim) :: Dense  (outputDim, f2)  ::  SquaredError(),
    settings = Settings[Double](
      updateRule = Vanilla(),
      batchSize = Some(100),
      iterations = 10000,
      learningRate = {
        case (iter, _) if iter < 128 => 0.00001
        case (_, _)  => 0.00001
      },
      precision = 1E-4
    )
  )

  override def selectMove(agent: A, e: Env): Action = {
    val stateVector = List(agent.id.toDouble)
    vectorToMove(net.evaluate(->(stateVector:_*)).toArray.toList)
  }
}
