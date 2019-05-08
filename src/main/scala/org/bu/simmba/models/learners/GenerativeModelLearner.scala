package org.bu.simmba.models.learners

import java.io.FileWriter

import org.bu.simmba.Turn
import org.bu.simmba.models.BipartiteModel
import org.bu.simmba.types.{Agent, Environment}
import play.api.libs.json._

/**
  * @param vectorToBoard - function converting a vector to the Environment type
  * @param vectorToMove - function converting a vector to the Action type
  * @param agentDim - number of dimensions in attribute space for agent type
  * @param possibleMoves - specifies space of actions (currently must be finite)
  * @tparam Env - Secifies space that contains agent(s)
  * @tparam A - Defines attributes for an agent
  * @tparam Action - Defines attributes for an action
  */
class GenerativeModelLearner[Env <: Environment[Action, A], A <: Agent ,Action](
                             val vectorToBoard: Seq[Turn] => Env,
                             val vectorToMove: Seq[Turn] => Action,
                             val agentDim: Int,
                             possibleMoves: Seq[Action],
                             possibleStates: Seq[Seq[Int]]) extends Learner[Env, A, Action]{

  override def learn(trainingDataFilePath: String, boardSize: Int,
                     numPlayers: Int, playerId: Int, paramsFile: String = ""): Unit = {
    val boardDim = numPlayers * agentDim + 2
    val moveDim = 2
    val data = getFeaturizedTrainingData(trainingDataFilePath, boardDim, moveDim)
    val numRows = boardSize
    val numCols = boardSize

    val playerData  = data.filter{ case (_, _, turn, winnerId) =>
      turn == playerId && winnerId == playerId
    }.map{ case (board, move, _, _) =>
      val p1State = board.p1.state
      val p2State = board.p2.state
      (List(playerId) ++ p1State ++ p2State, move)
    }.toList

    val playerModel = BipartiteModel(Seq(playerData), possibleStates, possibleMoves)
    val combinedJson: JsValue = JsObject(playerModel.asJson("_move").value)
    val paramsFileName = if(paramsFile == "") s"gen_model_${playerId}_${numRows}by$numCols.json" else paramsFile
    printJsonString(combinedJson, paramsFileName, append = false)
  }
  private def printJsonString(json: JsValue, outfile: String, append: Boolean = true): Unit = {
    val pw = new FileWriter(outfile, append)
    pw.append(s"$json\n")
    pw.close()
  }
}