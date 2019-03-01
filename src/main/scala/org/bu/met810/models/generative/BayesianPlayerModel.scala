package org.bu.met810.models.generative

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810.models.{BoardValidation, JsonModelLoader, PlayerModel}
import org.bu.met810.types.{Agent, Environment}

class BayesianPlayerModel[Env <: Environment[Action, A], A <: Agent, Action]
(val paramsFile: String, val useGenerativeParams: Boolean, possibleMoves: Seq[Action])
  extends PlayerModel[Env, A, Action] with JsonModelLoader with BoardValidation {

  private val modelParams = ModelParameters()
  paramsMap.map{case(k,v) => Dirichlet(v:_*)(k, modelParams)}

  override def selectMove(player: A, board: Env): Action = {
    Universe.createNew()
    val possiblePositions = List(player.positions.head)
    val (x1, y1) = player.positions.head
    val (x2, y2) = possiblePositions.head
    val moveDist = {
      val queryString = s"${player.id}_${List(x1, y1, x2, y2).mkString("_")}_move"
      val params = modelParams.getElementByReference(queryString).asInstanceOf[AtomicDirichlet]
      Select(params, possibleMoves: _*)
    }
    val alg = Importance(300, moveDist)
    alg.start()
    val computedMoves = alg.distribution(moveDist).toList
    val desiredMove = computedMoves.filter{case (_ ,m) => board.isValidAction(m, player)}.sortWith(_._1 > _._1).head._2
    if(!possibleMoves.contains(desiredMove))
      throw new NoSuchElementException(s"move $desiredMove does not belong in $player")
    alg.stop()
    alg.kill()
    desiredMove
  }
}

object BayesianPlayerModel{
  def apply[Env <: Environment[Action, A], A <: Agent, Action]
  (paramsFile: String, useGenerativeParams: Boolean, possibleMoves: Seq[Action]) =
    new BayesianPlayerModel[Env, A, Action](paramsFile,useGenerativeParams, possibleMoves)
}
