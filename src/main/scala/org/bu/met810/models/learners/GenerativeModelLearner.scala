package org.bu.met810.models.learners

import java.io.FileWriter

import org.bu.met810.models.BipartiteModel
import org.bu.met810.{Turn, permutationsWithRepetitions}
import org.bu.met810.types.{Agent, Environment}
import play.api.libs.json._


class GenerativeModelLearner[Env <: Environment[Action, A], A <: Agent ,Action](
                             override val vectorToBoard: Seq[Turn] => Env,
                             override val vectorToMove: Seq[Turn] => Action,
                             isValidState: Seq[Int] => Boolean,
                             possibleMoves: Seq[Action]) extends Learner[Env, A, Action]{

  override def learn(trainingDataFilePath: String, boardSize: Int,
                     numPlayers: Int, playerId: Int, paramsFile: String = ""): Unit = {
    val boardDim = numPlayers * 2 + 2
    val moveDim = 2
    val data = getFeaturizedTrainingData(trainingDataFilePath, boardDim, moveDim)
    val numRows = boardSize
    val numCols = boardSize

    val playerData  = data.filter{ case (_, _, turn, winnerId) =>
      turn == playerId && winnerId == playerId
    }.map{ case (board, move, _, _) =>
      val p1State = board.p1.positions.flatMap(p => List(p._1, p._2))
      val p2State = board.p2.positions.flatMap(p => List(p._1, p._2))
      (List(playerId) ++ p1State ++ p2State, move)
    }.toList

    val possiblePositions =
      permutationsWithRepetitions((0 until boardSize).toList, boardSize)
        .map{ state => List(playerId) ++ state}
        .filter(isValidState)

    val playerModel = BipartiteModel(Seq(playerData), possiblePositions, possibleMoves)
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