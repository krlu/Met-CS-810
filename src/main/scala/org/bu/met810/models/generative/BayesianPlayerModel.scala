package org.bu.met810.models.generative

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

class BayesianPlayerModel(val paramsFile: String, val useGenerativeParams: Boolean)
  extends PlayerModel[Board, Player, Move] with JsonModelLoader with BoardValidation {

  val modelParams = ModelParameters()
  paramsMap.map{case(k,v) => Dirichlet(v:_*)(k, modelParams)}

  override def selectMove(playerId: Int, board: Board): Move = {
    Universe.createNew()
    val (player, possiblePositions) = getPlayerData(playerId, board)
    val (x1, y1) = player.position
    val positionDist = Select(possiblePositions.head:_*)
    val moveDist = positionDist.flatMap { case (x2, y2) =>
      val queryString = s"${playerId}_${List(x1, y1, x2, y2).mkString("_")}_move"
      val params = modelParams.getElementByReference(queryString).asInstanceOf[AtomicDirichlet]
      Select(params, player.moves: _*)
    }
    val alg = Importance(300, moveDist)
    alg.start()
    val computedMoves = alg.distribution(moveDist).toList
    val desiredMove = computedMoves.filter{case (_ ,m) => validMoves(player, board).contains(m)}.sortWith(_._1 > _._1).head._2
    if(!player.moves.contains(desiredMove))
      throw new NoSuchElementException(s"move $desiredMove does not belong in $player")
    alg.stop()
    alg.kill()
    desiredMove
  }
}

object BayesianPlayerModel{
  def apply(paramsFile: String, useGenerativeParams: Boolean): BayesianPlayerModel =
    new BayesianPlayerModel(paramsFile,useGenerativeParams)
}
