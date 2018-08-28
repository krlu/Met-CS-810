package org.bu.met810.model

import neuroflow.application.plugin.IO._
import neuroflow.core.Activators.Double._
import neuroflow.core._
import neuroflow.nets.cpu.DenseNetwork._
import org.bu.met810.types.boardassets.Board
import org.bu.met810.types.moves.Move

//TODO: Current uses toy example from Neuroflow site
class AdversarialModel extends PlayerModel{
  implicit val weights: WeightBreeder[Double] = WeightBreeder[Double].normal(μ = 0.0, σ = 0.1)

  val (e, f) = (Some(Linear.biased(0.1)), ReLU)
  val settings: Settings[Double] = Settings[Double](
    updateRule = Vanilla(),
    batchSize = Some(8),
    iterations = 256,
    learningRate = {
      case (iter, α) if iter < 128 => 1E-4
      case (_, _)  => 1E-6
    },
    precision = 1E-8
  )

  override def selectMove(possibleMoves: Set[Move], board: Board): Move = ???

  val file = "/path/to/net.nf"
  val net = Network(net, settings)
  File.writeWeights(net.weights, file)
  val json: String = Json.writeWeights(net.weights)
}

