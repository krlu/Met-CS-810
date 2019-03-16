package org.bu.met810.models.learners

import neuroflow.application.plugin.IO.{File, _}
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.types.{Agent, Environment}
import org.bu.met810.{NNVector, Turn}

class NNLearner[Env <: Environment[Action, A], A <: Agent ,Action](inputDim: Int, outputDim: Int,
                                                                   savedWeights: Option[String],
                                                                   val useNoise: Boolean,
                                                                   val vectorToBoard: Seq[Turn] => Env,
                                                                   val vectorToMove: Seq[Turn] => Action,
                                                                   val agentDim: Int) extends Learner[Env, A, Action]{
  //  private val f1 = Activators.Double.Sigmoid
  private val f2 = Activators.Double.Linear

  implicit val weights: WeightBreeder[Double] = savedWeights match{
    case None => WeightBreeder[Double].normal(μ = 0.0, σ = 2.0)
    case Some(filePath) => File.weightBreeder(filePath)
  }

  val net = Network(
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

  override def learn(trainingDataFilePath: String, boardSize: Int, numPlayers: Int, playerId: Int, paramsFile: String): Unit = {
    val (xs, ys) = getNNTrainingData(trainingDataFilePath).map{ case (x,y, _, _) => (x,y)}.unzip[NNVector, NNVector]
    net.train(xs, ys)
    File.writeWeights(net.weights, paramsFile)
  }

}