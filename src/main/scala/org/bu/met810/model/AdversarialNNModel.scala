package org.bu.met810.model

import neuroflow.application.plugin.IO.File
import neuroflow.application.plugin.Notation.->
import neuroflow.core.Activators.Double.Sigmoid
import neuroflow.core._
import neuroflow.dsl.{Dense, Vector}
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

import scala.io.Source

class AdversarialNNModel(inputDim: Int = 10, outputDim: Int = 10, savedWeights: Option[String] = None) extends NNPlayerModel{

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

  override def selectMove(player: Player, board: Board): Move = {
    val inputVector = ->(vectorizeBoard(board) ++ vectorizeMovesForPlayer(player):_*)
    val outputVector = net.apply(inputVector)
    vectorToMove(outputVector.data.toSeq)
  }

  override def vectorizeBoard(board: Board): Seq[Double] = ???
  override def vectorizeMovesForPlayer(player: Player): Seq[Double] = ???
  override def vectorToMove(vector: Seq[Double]): Move = ???
}
