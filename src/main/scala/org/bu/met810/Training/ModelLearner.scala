package org.bu.met810.Training

import neuroflow.application.plugin.IO.File
import neuroflow.application.plugin.Notation._
import neuroflow.core.Activators.Double._
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._

import scala.io.Source

class ModelLearner {

  private type NNVector = _root_.neuroflow.core.Network.Vector[Double]

  private implicit val weights: WeightBreeder[Double] = WeightBreeder[Double].normal {
    Map ( // normal config per layer index
      1 -> (0.0, 1.0),
      2 -> (0.0, 0.1)
    )
  }

  private val settings: Settings[Double] = Settings[Double](
    learningRate = { case (_, _) => 1.0 },
    iterations = 100000)
  private val fn: Activator.Sigmoid[Double] = Sigmoid
  private val net = Network(
    layout = Vector(2) :: Dense(3, fn) :: Dense(1, fn) :: SquaredError(),
    settings = settings
  )

  def train(trainingDataPath: String, outputPath: String): Unit ={
    val (xs, ys) = setupTrainingData(trainingDataPath)
    net.train(xs, ys)
    File.writeWeights(net.weights, outputPath)
  }

  // TODO: Need to adjust this based on the scenario
  private def setupTrainingData(fileName: String): (Seq[NNVector], Seq[NNVector]) ={
    var trainingInput: Seq[NNVector] = Seq()
    var trainingOutput: Seq[NNVector] = Seq()
    val bufferedSource = Source.fromFile(fileName)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim.toDouble)
      val input = ->(cols.dropRight(3):_*)
      val output = ->(cols(97), cols(98), cols(99))
      trainingInput = trainingInput :+ input
      trainingOutput = trainingOutput :+ output
    }
    bufferedSource.close
    (trainingInput, trainingOutput)
  }
}

