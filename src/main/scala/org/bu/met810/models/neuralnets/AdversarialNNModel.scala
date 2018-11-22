package org.bu.met810.models.neuralnets

import neuroflow.application.plugin.IO.{File, _}
import neuroflow.application.plugin.Notation.->
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.{Down, Left, Move, Right, SkipDown, SkipLeft, SkipRight, SkipUp, Up}

import scala.io.Source

class AdversarialNNModel(inputDim: Int = 10, outputDim: Int = 10, savedWeights: Option[String] = None) extends NNPlayerModel[Board, Player, Move]{

  private type NNVector = _root_.neuroflow.core.Network.Vector[Double]
  private val f = Activators.Double.Sigmoid//Activators.Double.LeakyReLU(0.0)
  implicit val weights: WeightBreeder[Double] = WeightBreeder[Double].normal(μ = 0.0, σ = 2.0)

  val net = Network(
    layout = Vector (2) :: Dense  (3, f)  :: Dense  (1, f)  ::  SquaredError(),
    settings = Settings[Double](
      updateRule = Vanilla(),
      batchSize = Some(4),
      iterations = 100000,
      learningRate = {
        case (iter, α) if iter < 128 => 1.0
        case (_, _)  => 0.5
      },
      precision = 1E-4
    )
  )
  def train(trainingDataPath: String, outputPath: String): Unit ={
    val (xs, ys) = setupTrainingData(trainingDataPath)
    net.train(xs, ys)
    File.writeWeights(net.weights, outputPath)
  }

  private def setupTrainingData(fileName: String): (Seq[NNVector], Seq[NNVector]) ={
    var trainingInput: Seq[NNVector] = Seq()
    var trainingOutput: Seq[NNVector] = Seq()
    val bufferedSource = Source.fromFile(fileName)
    for (line <- bufferedSource.getLines) {
      val cols = line.split(",").map(_.trim.toDouble)
      val input = ->(cols.dropRight(outputDim):_*)
      val output = ->(cols.drop(inputDim):_*)
      trainingInput = trainingInput :+ input
      trainingOutput = trainingOutput :+ output
    }
    bufferedSource.close
    (trainingInput, trainingOutput)
  }

  override def selectMove(playerId: Int, board: Board): Move = {
    val inputVector = ->(board.toVector:_*)
    val outputVector = net.evaluate(inputVector)
    vectorToMove(outputVector.data.toList)
  }
  override def vectorToMove(vector: Seq[Double]): Move =
    Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight).find(_.toVector == vector.map(_.ceil.toInt)) match {
      case Some(move) => move
      case None => throw new NoSuchElementException(s"unable to find move with vector ${vector.map(_.ceil.toInt)}!")
    }
}
