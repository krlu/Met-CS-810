package org.bu.met810.model

import breeze.linalg.DenseMatrix
import neuroflow.application.plugin.IO.{File, _}
import neuroflow.application.plugin.Notation.->
import neuroflow.application.processor.Image.TensorRGB
import neuroflow.core._
import neuroflow.dsl.Convolution.autoTupler
import neuroflow.dsl.{Convolution, Dense}
import neuroflow.nets.cpu.ConvNetwork._
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.{Down, Left, Move, Right, SkipDown, SkipLeft, SkipRight, SkipUp, Up}

import scala.io.Source

class AdversarialNNModel(inputDim: Int = 10, outputDim: Int = 10, savedWeights: Option[String] = None) extends NNPlayerModel[Board, Player, Move]{

  private type NNVector = _root_.neuroflow.core.Network.Vector[Double]

  val μ = 0.0
  implicit val weights: WeightBreeder[Double] = WeightBreeder[Double].normal(Map(
    0 -> (μ, 0.1),  1 -> (μ, 0.1), 2 -> (μ, 0.1), 3 -> (μ, 0.01)
  ))

  private val f = Activators.Double.LeakyReLU(0.0)
  val c0 = Convolution(dimIn = (inputDim, inputDim, 1),  padding = 1, field = 3, stride = 1, filters = 128, activator = f)
  val c1 = Convolution(dimIn = c0.dimOut, padding = 1, field = 3, stride = 1, filters = 128, activator = f)
  val c2 = Convolution(dimIn = c1.dimOut, padding = 1, field = 4, stride = 2, filters = 128, activator = f)

  private val net = Network(
    layout = c0 :: c1 :: c2  :: Dense(10, f) :: SoftmaxLogEntropy(),
    Settings[Double](
      prettyPrint     = true,
      learningRate    = {
        case (i, _) if i < 4000 => 1E-5
        case (_, _)             => 1E-6
      },
      updateRule      = Momentum(μ = 0.8f),
      iterations      = Int.MaxValue,
      precision       = 1E-2
    )
  )

  def train(trainingDataPath: String, outputPath: String): Unit ={
    val (xs, ys) = setupTrainingData(trainingDataPath)
    net.train(xs, ys)
    File.writeWeights(net.weights, outputPath)
  }
  private def setupTrainingData(fileName: String): (Seq[Tensor3D[Double]], Seq[NNVector]) ={
    var trainingInput: Seq[Tensor3D[Double]] = Seq()
    var trainingOutput: Seq[NNVector] = Seq()
    val bufferedSource = Source.fromFile(fileName)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim.toDouble)
      val matrix = DenseMatrix(List.fill(inputDim)(cols.dropRight(outputDim)):_*)
      println(matrix)
      println()
      val input = new TensorRGB[Double](inputDim, inputDim, matrix)
      val output = ->(cols.drop(inputDim):_*)
      trainingInput = trainingInput :+ input
      trainingOutput = trainingOutput :+ output
    }
    bufferedSource.close
    (trainingInput, trainingOutput)
  }

  override def selectMove(playerId: Int, board: Board): Move = {
    val inputVector = ->(board.toVector:_*)
    val outputVector = net.evaluate(new TensorRGB[Double](1,1, DenseMatrix(List(0.0))))
    vectorToMove(outputVector.data.toList)
  }
  override def vectorToMove(vector: Seq[Double]): Move =
    Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight).find(_.toVector == vector.map(_.ceil.toInt)) match {
      case Some(move) => move
      case None => throw new NoSuchElementException(s"unable to find move with vector ${vector.map(_.ceil.toInt)}!")
    }
}
