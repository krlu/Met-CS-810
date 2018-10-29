package org.bu.met810

import org.bu.met810.data.Simulator
import org.bu.met810.model.bayes.BayesianPlayerModel
import org.bu.met810.model.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move
import org.scalatest.{FlatSpec, Matchers}

class PlayerModelTest extends FlatSpec with Matchers {

  "random robber model" should "win infrequently" in {
    val board = Board(Robber((1, 1)), Cop((0, 0)), 3, 3, Seq.empty[Building])
    println(runExperiment(RandomMoveModel(), board))
  }

  "NN robber model" should "win often" in {
    val board = Board(Robber((1, 1)), Cop((0, 0)), 3, 3, Seq.empty[Building])
    val model = new BayesianPlayerModel("model_0.json")
    println(runExperiment(model, board))
  }

  private def runExperiment(model: PlayerModel[Board, Player, Move], board: Board): (Int, Int) = {
    val winners: Seq[Player] = for(i <- 1 to 10) yield {
      val sim = Simulator(board, model, RandomMoveModel())
      println(i)
      sim.runFullGame()
    }
    val robbers = winners.filter(p => p.isInstanceOf[Robber])
    val cops = winners.filter(p => p.isInstanceOf[Cop])
    (robbers.size, cops.size)
  }
}
