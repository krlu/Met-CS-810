package org.bu.met810.model.bayes

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.Select
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810.model.PlayerModel
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

class BayesianPlayerModel(paramsMap: Map[String, List[Double]])extends PlayerModel[Board, Player, Move]{

  val modelParams = ModelParameters()
  paramsMap.map{ case(k,v) => Dirichlet(v:_*)(k, modelParams)}

  override def selectMove(playerId: Int, board: Board): Move = {
    val player: Player = Set(board.p1, board.p2).find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    val (x1, y1) = board.p1.position
    val (x2, y2) = board.p2.position
    val params = modelParams.getElementByReference(s"${playerId}_${List(x1,y1,x2,y2).mkString("_")}").asInstanceOf[AtomicDirichlet]
    val moveDist = Select(params, player.moves:_*)
    val alg = Importance(1000, moveDist)
    alg.start()
    val computedDist = alg.distribution(moveDist).sortWith(_._1 > _._1)
    val desiredMove = computedDist.head._2
    alg.stop()
    desiredMove
  }
}
