package org.bu.met810.models.neuralnets

import neuroflow.application.plugin.IO.File
import neuroflow.application.plugin.Notation.->
import neuroflow.core._
import neuroflow.dsl._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.{Down, Left, Move, Right, SkipDown, SkipLeft, SkipRight, SkipUp, Up}
import org.bu.met810.choose

class NNModel(inputDim: Int = 6, outputDim: Int = 2, savedWeights: Option[String] = None) extends PlayerModel[Board, Player, Move]{

//  private val f1 = Activators.Double.Sigmoid
  private val f2 = Activators.Double.Linear

  implicit val weights: WeightBreeder[Double] = savedWeights match {
    case None => WeightBreeder[Double].normal(μ = 0.0, σ = 2.0)
    case Some(filePath) => File.weightBreeder(filePath)
  }

  val net = Network(layout = Vector(inputDim) :: Dense(outputDim, f2) :: Dense(outputDim, f2) :: SquaredError())

  override def selectMove(playerId: Int, board: Board): Move = {
    val inputVector = ->(board.toVector:_*)
    val possibleMoves = Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight)
    val outputVector = net.evaluate(inputVector)
    var moveOpt: Option[Move] = possibleMoves.find(_.toVector == outputVector.data.toList.map(_.ceil.toInt))
    println(outputVector.data.toList.map(_.ceil.toInt))
    println(moveOpt.getOrElse(choose(possibleMoves.iterator)))
    moveOpt.getOrElse(choose(possibleMoves.iterator))
  }
}
