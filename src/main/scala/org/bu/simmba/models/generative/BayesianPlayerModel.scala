package org.bu.simmba.models.generative

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.simmba.models.{JsonModelLoader, PlayerModel}
import org.bu.simmba.types.{Agent, Environment}

class BayesianPlayerModel[Env <: Environment[Action, A], A <: Agent, Action]
(val paramsFile: String, val useGenerativeParams: Boolean, possibleMoves: Seq[Action])
  extends PlayerModel[Env, A, Action] with JsonModelLoader {

  private val modelParams = ModelParameters()
  paramsMap.map{case(k,v) => Dirichlet(v:_*)(k, modelParams)}

  override def selectMove(player: A, board: Env): Action = {
    Universe.createNew()
    val p1State = board.p1.state
    val p2State = board.p2.state
    val queryString = s"${(List(player.id) ++ p1State ++ p2State).mkString("_")}_move"
    val moveDist = {
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

  override def modelName: String = "Bayesian"
}

object BayesianPlayerModel{
  def apply[Env <: Environment[Action, A], A <: Agent, Action]
  (paramsFile: String, useGenerativeParams: Boolean, possibleMoves: Seq[Action]) =
    new BayesianPlayerModel[Env, A, Action](paramsFile,useGenerativeParams, possibleMoves)
}
