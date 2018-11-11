package org.bu.met810

import com.cra.figaro.language.Universe
import org.bu.met810.data.Simulator
import org.bu.met810.model.bayes.BayesianPlayerModel
import org.bu.met810.model.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move
import org.scalatest.{FlatSpec, Matchers}

class PlayerModelTest extends FlatSpec with Matchers {

  val board = Board(Robber((0, 0)), Cop((3, 3)), 4, 4, Seq.empty[Building])
  "Random robber model" should "win infrequently" in {
    println(runExperiment(RandomMoveModel(), board))
  }

  "Bayesian robber model" should "win often" in {
    val model = new BayesianPlayerModel("model_0_4by4_v3.json")
    println(runExperiment(model, board))
  }

  private def runExperiment(model: PlayerModel[Board, Player, Move], board: Board): (Int, Int) = {
    val start = System.currentTimeMillis()
    val winners: Seq[Player] = for(i <- 1 to 1000) yield {
      Universe.createNew()
      val sim = Simulator(board, model, RandomMoveModel())
      println(i)
      sim.runFullGame()
    }
    val robbers = winners.filter(p => p.isInstanceOf[Robber])
    val cops = winners.filter(p => p.isInstanceOf[Cop])
    val end = System.currentTimeMillis()
    println(s"runtime: ${(end - start)/1000}")
    (robbers.size, cops.size)
  }
}
