package org.bu.met810.models.learners

import java.io.PrintWriter

import argonaut.Argonaut._
import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810._
import org.bu.met810.models.JsonModelLoader
import org.bu.met810.types.{Agent, Environment}

/**
  * @param vectorToBoard - function converting a vector to the Environment type
  * @param vectorToMove - function converting a vector to the Action type
  * @param paramsFile - for saving learned parameters
  * @param useGenerativeParams - generative params and bayesian params JSON are parsed differently
  * @param agentDim - number of dimensions in attribute space for agent type
  * @param isValidState - function determining if agent state if valid in the environment
  * @param possibleMoves - specifies space of actions (currently must be finite)
  * @tparam Env - Secifies space that contains agent(s)
  * @tparam A - Defines attributes for an agent
  * @tparam Action - Defines attributes for an action
  */
class BayesianModelLearner[Env <: Environment[Action, A], A <: Agent ,Action](
                               override val vectorToBoard: Seq[Turn] => Env,
                               override val vectorToMove: Seq[Turn] => Action,
                               override val paramsFile: String,
                               override val useGenerativeParams: Boolean,
                               override val agentDim: Int,
                               isValidState: Seq[Int] => Boolean,
                               possibleMoves: Seq[Action]) extends Learner[Env, A, Action] with JsonModelLoader {

  def learn(trainingDataFilePath: String, boardSize: Int, numPlayers: Int, playerId: Int, paramsFile: String): Unit = {
    val numRows = boardSize
    val numCols = boardSize
    val boardDim = numPlayers * agentDim + 2
    val moveDim = 2

    def train(data: List[(Env, Action)], playerId: Int, useLearnedParams: Boolean): String = {
      Universe.createNew()
      val modelParams = ModelParameters()
      /**
        * @param board - boardState
        * @param move - observed move for player model
        */
      def generateTrial(board: Env, move: Action, playerId: Int): Unit = {
        val p1State = board.p1.state
        val p2State = board.p2.state
        val queryString = s"${playerId}_${(List(playerId) ++ p1State ++ p2State).mkString("_")}_move"
        val params = modelParams.getElementByReference(queryString).asInstanceOf[AtomicDirichlet]
        val moveDist = Select(params, possibleMoves:_*)
        moveDist.observe(move)
      }

      if(useLearnedParams) paramsMap.map{case(k,v) => Dirichlet(v:_*)(k, modelParams)}.toList
      else {
        possiblePositions(numRows, numCols, agentDim * numPlayers).map{ state =>
          val name = s"${playerId}_${state.mkString("_")}_move"
          Dirichlet(Array.fill(possibleMoves.size)(1.0):_*)(name, modelParams)
        }
      }
      data.foreach{ case(p, m) => generateTrial(p,m, playerId) }
      val alg = EMWithVE(2, modelParams)
      alg.start()
      val savedParams = modelParams.asJson.toString()
      alg.stop()
      alg.kill()
      savedParams
    }

    def trainForPlayer(playerId: Int): Unit = {
      val data: Seq[(Env, Action, Turn, WinnerId)] = getFeaturizedTrainingData(trainingDataFilePath, boardDim, moveDim)
      val playerSpecificData: Seq[(Env, Action)] = data.filter{ case (_, _, turn, winnerId) =>
        turn == playerId && winnerId == playerId
      }.map{ case (board, move, _, _) =>
        (board, move)
      }.toList

      val chunkedData = playerSpecificData.grouped(20).toList
      chunkedData.indices.foreach{ i =>
        val row = chunkedData(i).toList
        val useParamsFile = i > 0
        val paramsString = train(row, playerId, useParamsFile)
        val pw = new PrintWriter(paramsFile)
        pw.println(paramsString)
        pw.close()
      }
    }

    val start = System.currentTimeMillis()
    trainForPlayer(playerId)
    val end = System.currentTimeMillis()
    println(s"Training time: ${(end - start)/1000.0}s")
  }
}