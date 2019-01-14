package org.bu.met810.models.inference

import java.io.PrintWriter

import argonaut.Argonaut._
import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810._
import org.bu.met810.models.{BoardValidation, JsonModelLoader}
import org.bu.met810.types.copsandrobbersassets.{Board, Down, Move, SkipDown, SkipLeft, SkipRight, SkipUp, Up}


/** Given the dimensions of the board we build up our initial distributions*/
class BayesianModelLearner(val paramsFile: String, val useGenerativeParams: Boolean)
  extends JsonModelLoader with BoardValidation with Learner {

  private val allMoves = List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)

  def learn(trainingDataFilePath: String, boardSize: Int, numPlayers: Int, playerId: Int, paramsFile: String): Unit = {
    val numRows = boardSize
    val numCols = boardSize
    val boardDim = numPlayers * 2 + 2 //num players * num coordinates + 1 for board length + 1 for board width
    val moveDim = 2

    def train(data: List[(Board, Move)], playerId: Int, useLearnedParams: Boolean): String = {
      Universe.createNew()
      val modelParams = ModelParameters()
      if(useLearnedParams) paramsMap.map{case(k,v) => Dirichlet(v:_*)(k, modelParams)}.toList
      else {
        permutationsWithRepetitions(possiblePositions(numRows, numCols), numPlayers).map{ positions =>
          val name = s"${playerId}_${positions.flatMap{case(a,b) => List(a,b)}.mkString("_")}_move"
          Dirichlet(Array.fill(allMoves.size)(1.0):_*)(name, modelParams)
        }
      }
      /**
        * TODO: replace boardState with vector of positions
        * @param board - boardState
        * @param move - observed move for player model
        */
      def generateTrial(board: Board, move: Move, playerId: Int): Unit = {
        val (player, possiblePositions) = getPlayerData(playerId, board)
        val (x1, y1) = player.position
        val (x2, y2) = possiblePositions.head
        val queryString = s"${playerId}_${List(x1, y1, x2, y2).mkString("_")}_move"
        val params = modelParams.getElementByReference(queryString).asInstanceOf[AtomicDirichlet]
        val moveDist = Select(params, allMoves:_*)
        moveDist.observe(move)
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
      val data = getFeaturizedTrainingData(trainingDataFilePath, boardDim, moveDim)
      val playerSpecificData: List[(Board, Move)] = data.filter{ case (_, _, turn, winnerId) =>
        turn == playerId && winnerId == playerId
      }.map{ case (board, move, _, _) =>
        (board, move)
      }.toList
      val chunkedData = playerSpecificData.grouped(20).toList
      chunkedData.indices.foreach{ i =>
        val row = chunkedData(i)
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

object BayesianModelLearner{
  def apply(paramsFile: String, useGenerativeParams: Boolean): BayesianModelLearner =
    new BayesianModelLearner(paramsFile, useGenerativeParams)
}
