package org.bu.met810.models.inference

import java.io.FileWriter

import org.bu.met810.models.BipartiteModel
import org.bu.met810.types.boardassets.Board
import org.bu.met810.types.moves.{Down, Left, Move, Right, SkipDown, SkipLeft, SkipRight, SkipUp, Up}
import org.bu.met810.{Turn, WinnerId}
import play.api.libs.json._


class GenerativeModelLearner extends Learner{

  override def learn(trainingDataFilePath: String, boardSize: Turn, numPlayers: Turn, playerId: Turn, paramsFile: String = ""): Unit = {
    val start = System.currentTimeMillis()
    val boardDim = numPlayers * 2 + 2
    val moveDim = 2

    val data: Seq[(Board, Move, Turn, WinnerId)] = getFeaturizedTrainingData(trainingDataFilePath, boardDim, moveDim)

    val numRows = boardSize
    val numCols = boardSize

    val playerData  = data.filter{ case (_, _, turn, winnerId) =>
      turn == playerId && winnerId == playerId
    }.map{ case (board, move, _, _) =>
      val (a, b) = board.p1.position
      val (c, d) = board.p2.position
      ((playerId, a, b, c, d), move)
    }.toList

    val possiblePositions = {
      for{
        a <- 0 until boardSize
        b <- 0 until boardSize
        c <- 0 until boardSize
        d <- 0 until boardSize
      } yield{
        (playerId, a, b, c, d)
      }
    }.filter{ case (_, x1, y1, x2, y2) => x1 != x2 || y1 != y2}

    val possibleMoves = List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
    println("training player model...")
    val playerModel = BipartiteModel(Seq(playerData), possiblePositions, possibleMoves)
    val combinedJson: JsValue = JsObject(playerModel.asJson("_move").value)
    val paramsFileName = if(paramsFile == "") s"gen_model_${playerId}_${numRows}by$numCols.json" else paramsFile
    printJsonString(combinedJson, paramsFileName, append = false)
    val end = System.currentTimeMillis()
    println(s"Training time: ${(end - start)/1000.0}s")
  }
  private def printJsonString(json: JsValue, outfile: String, append: Boolean = true): Unit ={
    val pw = new FileWriter(outfile, append)
    pw.append(s"$json\n")
    pw.close()
  }
}

object GenerativeModelLearner{
  def apply(): GenerativeModelLearner = new GenerativeModelLearner()
}
