package org.bu.met810.models.neuralnets

import neuroflow.application.plugin.IO.File
import neuroflow.application.plugin.Notation.->
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.{Down, Left, Move, Right, SkipDown, SkipLeft, SkipRight, SkipUp, Up}

class AdversarialNNModel(inputDim: Int = 10, outputDim: Int = 10, savedWeights: Option[String] = None) extends PlayerModel[Board, Player, Move]{

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
        case (iter, _: Double) if iter < 128 => 1.0
        case (_, _)  => 0.5
      },
      precision = 1E-4
    )
  )

  override def selectMove(playerId: Int, board: Board): Move = {
    val inputVector = ->(board.toVector:_*)
    val outputVector = net.evaluate(inputVector)
    vectorToMove(outputVector.data.toList)
  }

  private def vectorToMove(vector: Seq[Double]): Move =
    Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight).find(_.toVector == vector.map(_.ceil.toInt)) match {
      case Some(move) => move
      case None => throw new NoSuchElementException(s"unable to find move with vector ${vector.map(_.ceil.toInt)}!")
    }
}
