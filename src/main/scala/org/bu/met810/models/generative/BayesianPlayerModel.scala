package org.bu.met810.models.generative

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

class BayesianPlayerModel(val paramsFile: String, val useGenerativeParams: Boolean)
  extends PlayerModel[Board, Player, Move] with JsonModelLoader with BoardValidation{

  val modelParams = ModelParameters()
  paramsMap.map{case(k,v) => Dirichlet(v:_*)(k, modelParams)}

  override def selectMove(playerId: Int, board: Board): Move = {
    Universe.createNew()
    val player: Player = Set(board.p1, board.p2).find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    val (x1, y1) = board.p1.position
    val p2PosDist = Select(applyNoise(board.p2.position, positionRadius = 1, minFactor = 0.5):_*)
    val moveDist = p2PosDist.flatMap{ case (x2, y2) =>
      val queryString = s"${playerId}_${List(x1,y1,x2,y2).mkString("_")}_move"
      val params = modelParams.getElementByReference(queryString).asInstanceOf[AtomicDirichlet]
      Select(params, player.moves:_*)
    }
    val alg = Importance(300, moveDist)
    alg.start()
    val computedDist = alg.distribution(moveDist).toList
    val desiredMove = computedDist.filter{case (_ ,m) => validMoves(player, board).contains(m)}.sortWith(_._1 > _._1).head._2
    alg.stop()
    alg.kill()
    desiredMove
  }

  private def applyNoise(pos: (Int, Int), positionRadius: Int, minFactor: Double): List[(Double, (Int, Int))] = {
    val (x, y) = pos
    for{
      xDelta <- -positionRadius to positionRadius
      yDelta <- -positionRadius to positionRadius
    } yield (1.0/ Math.max(minFactor, Math.hypot(xDelta, yDelta)), (x + xDelta, y + yDelta))
  }.toList
}
