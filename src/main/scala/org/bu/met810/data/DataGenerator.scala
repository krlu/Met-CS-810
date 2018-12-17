package org.bu.met810.data

import java.io.{File, FileWriter}

import org.bu.met810.models.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move
import org.bu.met810.{Turn, WinnerId, _}


/**
  *  Save sample if and only if
  *  - sample does not yet exist in training set AND one of the following
  *  - if game ended playerModel to train was the winner
  *  - if game not needed move leads to state that is in training set
  */
object DataGenerator {

  def generateData(outputFilePath: String, boardSize: Int, numSamples: Int = 4000,
                   shouldApplyNoise: Boolean = false, numPlayers: Int = 2, playerId: Int = 0,
                   p1Model: PlayerModel[Board, Player, Move] = RandomMoveModel(),
                   p2Model: PlayerModel[Board, Player, Move] = RandomMoveModel()): Unit ={
    val start = System.currentTimeMillis()
    val possiblePositions = possibleDifferentPositions(boardSize, boardSize, numPlayers)
    for{
      _ <- 1 to numSamples
      pos <- possiblePositions
    }{
      val p1Pos = pos.head
      val p2Pos = pos(1)
      val board = Board(Robber(p1Pos), Cop(p2Pos), boardSize, boardSize, Seq.empty[Building])
      generateDataPoint(playerId, outputFilePath, board, p1Model, p2Model, shouldApplyNoise)
    }
    val end = System.currentTimeMillis()
    println(s"Data generation time: ${(end - start)/1000.0}s")
  }

  private def generateDataPoint(playerId: Int, outputFilePath: String, initialBoard: Board,
                                p1Model: PlayerModel[Board, Player, Move],
                                p2Model: PlayerModel[Board, Player, Move],
                                shouldApplyNoise: Boolean): Unit = {
    var data = List.empty[(Board, Move, Turn)]

    val sim = Simulator(initialBoard, p1Model, p2Model)
    var result: Option[(Board, Move, Board)] = None
    var prevTurn = if(sim.turn == 0) 1 else 0
    while(!sim.isGameOver){
      result = sim.runSimulator()
      if(result.nonEmpty) {
        val (prevBoard, move, _) = result.get
        data = data :+ (prevBoard, move, prevTurn)
      }
      prevTurn = if(sim.turn == 0) 1 else 0
    }
    val winnerId: WinnerId = sim.getWinner.get.id
    if(winnerId == playerId) {
      data.foreach { case (board, move, turn) =>
        if(shouldApplyNoise){
          val (_, pos) = choose(applyNoise(board.p2.position, 1, 0.5))
          val newP2 = board.p2.asInstanceOf[Cop].copy(pos)
          val newBoard = board.copy(p2 = newP2)
          saveVectors(outputFilePath, newBoard.toVector, move.toVector, turn, winnerId)
        }
        else saveVectors(outputFilePath, board.toVector, move.toVector, turn, winnerId)
      }
    }
  }

  private def saveVectors(filePath: String, boardVec: Seq[Double], moveVec: Seq[Double], turn: Int, winnerId: WinnerId): Unit ={
    val pw = new FileWriter(new File(filePath), true)
    pw.append(s"${boardVec.mkString(",")},${moveVec.mkString(",")},$turn,$winnerId \n")
    pw.close()
  }
}
