package org.bu.met810.data

import java.io.{File, FileWriter}

import org.bu.met810.types.Vectorizable
import org.bu.met810.{Turn, WinnerId, _}


/**
  *  Save sample if and only if
  *  - sample does not yet exist in training set AND one of the following
  *  - if game ended playerModel to train was the winner
  *  - if game not needed move leads to state that is in training set
  */
object DataGenerator{

  def generateData[Env <: Vectorizable, Agent<: Vectorizable, Action <: Vectorizable]
  (outputFilePath: String, boardSize: Int, numSamples: Int, numPlayers: Int, playerId: Int,
   sim: Env => Simulator[Env, Agent, Action], func: Any => Env): Unit ={
    val start = System.currentTimeMillis()
    val possiblePositions = possibleDifferentPositions(boardSize, boardSize, numPlayers)
    for{
      _ <- 1 to numSamples
      pos <- possiblePositions
    }{
      val p1Pos = pos.head
      val p2Pos = pos(1)
      val board = func(p1Pos, p2Pos, boardSize)
      generateDataPoint(playerId, outputFilePath, sim(board))
    }
    val end = System.currentTimeMillis()
    println(s"Data generation time: ${(end - start)/1000.0}s")
  }

  private def generateDataPoint[Env <: Vectorizable, Agent<: Vectorizable, Action <: Vectorizable]
  (playerId: Int, outputFilePath: String, sim: Simulator[Env, Agent, Action]): Unit = {
    var data = List.empty[(Env, Action, Turn)]
    var result: Option[(Env, Action, Env)] = None
    var prevTurn = if(sim.turn == 0) 1 else 0
    while(!sim.isGameOver){
      result = sim.runSimulator()
      if(result.nonEmpty) {
        val (prevState, action, _) = result.get
        data = data :+ (prevState, action, prevTurn)
      }
      prevTurn = if(sim.turn == 0) 1 else 0
    }
    val winnerId: WinnerId = sim.getWinner.get.id
    if(winnerId == playerId)
      data.foreach { case (state, action, turn) =>
        saveVectors(outputFilePath, state.toVector, action.toVector, turn, winnerId)
      }
  }

  private def saveVectors(filePath: String, stateVec: Seq[Double], moveVec: Seq[Double], turn: Int, winnerId: WinnerId): Unit ={
    val pw = new FileWriter(new File(filePath), true)
    pw.append(s"${stateVec.mkString(",")},${moveVec.mkString(",")},$turn,$winnerId \n")
    pw.close()
  }
}
