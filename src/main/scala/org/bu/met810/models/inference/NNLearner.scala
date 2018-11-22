package org.bu.met810.models.inference

import neuroflow.application.plugin.IO.{File, _}
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.NNVector

protected class NNLearner(inputDim: Int = 10, outputDim: Int = 10, savedWeights: Option[String] = None) extends Learner {

  //  private val f1 = Activators.Double.Sigmoid
  private val f2 = Activators.Double.Linear

  implicit val weights: WeightBreeder[Double] = savedWeights match{
    case None => WeightBreeder[Double].normal(μ = 0.0, σ = 2.0)
    case Some(filePath) => File.weightBreeder(filePath)
  }

  val net = Network(
    layout = Vector (inputDim) :: Dense  (outputDim, f2)  :: Dense  (outputDim, f2)  ::  SquaredError(),
    settings = Settings[Double](
      updateRule = Vanilla(),
      batchSize = Some(4),
      iterations = 10000,
      learningRate = {
        case (iter, _) if iter < 128 => 1.0
        case (_, _)  => 0.5
      },
      precision = 1E-4
    )
  )

  override def learn(trainingDataFilePath: String, boardSize: Int, numPlayers: Int, playerId: Int): Unit = {
    val (xs, ys) = getNNTrainingData(trainingDataFilePath).map{ case (x,y, _, _) => (x,y)}.unzip[NNVector, NNVector]
    net.train(xs, ys)
    File.writeWeights(net.weights, "")
  }
}

object NNLearner{
  def apply(inputDim: Int = 10, outputDim: Int = 10, savedWeights: Option[String] = None): NNLearner = new NNLearner(
    inputDim,
    outputDim,
    savedWeights
  )
}
