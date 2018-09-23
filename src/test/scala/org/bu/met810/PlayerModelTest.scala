package org.bu.met810

import org.bu.met810.data.Simulator
import org.bu.met810.model.neuralnets.AdversarialNNModel
import org.bu.met810.model.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move
import org.scalatest.{FlatSpec, Matchers}

class PlayerModelTest extends FlatSpec with Matchers {

  "random robber model" should "win infrequently" in {
    val board = Board(Robber((6, 6)), Cop((9, 9)), 10, 10, Seq.empty[Building])
    println(runExperiment(RandomMoveModel(), board))
  }

  "NN robber model" should "win often" in {
    val board = Board(Robber((6, 6)), Cop((9, 9)), 10, 10, Seq.empty[Building])
    val nnModel = new AdversarialNNModel(inputDim = board.toVector.size, outputDim = 2, savedWeights = None)
    nnModel.train("training_data.csv", "saved_weights.nf")
    println(runExperiment(nnModel, board))
  }

  private def runExperiment(model: PlayerModel[Board, Player, Move], board: Board): Int = {
    val winners = for(_ <- 1 to 1000) yield{
      val sim = Simulator(board, model, RandomMoveModel())
      sim.runFullGame()
    }
    val robbers = winners.filter(p => p.isInstanceOf[Robber])
    robbers.size
  }
}
