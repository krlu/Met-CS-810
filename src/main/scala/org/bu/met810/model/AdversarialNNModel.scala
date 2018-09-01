package org.bu.met810.model

import neuroflow.application.plugin.IO.File
import neuroflow.application.plugin.Notation.->
import neuroflow.core.Activators.Double.Sigmoid
import neuroflow.core._
import neuroflow.dsl.{Dense, Vector}
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.{Down, Left, Move, Right, SkipDown, SkipLeft, SkipRight, SkipUp, Up}

import scala.io.Source

class AdversarialNNModel(inputDim: Int = 10, outputDim: Int = 10, savedWeights: Option[String] = None) extends NNPlayerModel[Board, Player, Move]{

  private type NNVector = _root_.neuroflow.core.Network.Vector[Double]

  private implicit val weights: WeightBreeder[Double] = savedWeights match {
    case Some(file) =>  File.weightBreeder[Double](file)
    case None =>  WeightBreeder[Double].normal {
      Map (
        1 -> (0.0, 1.0),
        2 -> (0.0, 0.1)
      )}
  }

  private val settings: Settings[Double] = Settings[Double](
    learningRate = { case (_, _) => 1.0 },
    iterations = 100000)
  private val fn: Activator.Sigmoid[Double] = Sigmoid
  private val net = Network(
    layout = Vector(inputDim) :: Dense(inputDim, fn) :: Dense(outputDim, fn) :: SquaredError(),
    settings = settings
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
    val inputVector = ->(board.toVector ++ Seq(playerId.toDouble):_*)
    val outputVector = net.apply(inputVector)
    vectorToMove(outputVector.data.toSeq)
  }
  override def vectorToMove(vector: Seq[Double]): Move =
    Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight).find(_.id == vector.head.toInt) match {
      case Some(move) => move
      case None => throw new NoSuchElementException(s"unable to find move with id ${vector.head}!")
    }
}
