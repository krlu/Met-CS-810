package org.bu.met810.data

import java.io.{File, FileWriter}

import org.bu.met810.types.{Environment, Vectorizable}
import org.bu.met810.{Turn, WinnerId}


/**
  * If the player with id=PlayerId wins, we save all moves from that game as training data
  */
object DataGenerator{

  /**
    * @param outputFilePath - csv file to contain training data
    * @param boardSize - rows and columns of a square board
    * @param numSamples - number of samples per unique board state
    * @param numPlayers - number of agents
    * @param playerId - Id of agent to generate data for
    * @param sim - generic builder for simulator
    * @param enumStatesFunc - generic enumerator for all possible game states
    * @tparam Env - represents game environment
    * @tparam Agent - represents agents interacting with game environment
    * @tparam A - represents action an agent can take
    */
  def generateData[Env <: Environment[A, Agent] with Vectorizable, Agent <: Vectorizable, A <: Vectorizable]
  (outputFilePath: String, boardSize: Int, numSamples: Int, numPlayers: Int,
   playerId: Int, sim: Env => Simulator[Env, Agent, A],
   enumStatesFunc: (Int, Int, Int) => List[Env]): Unit = {
    val start = System.currentTimeMillis()
    val possibleStates: Seq[Env] = enumStatesFunc(boardSize, boardSize, numPlayers)
    for{
      _ <- 1 to numSamples
      state <- possibleStates
    }{
      generateDataPoint(playerId, outputFilePath, sim(state))
    }
    val end = System.currentTimeMillis()
    println(s"Data generation time: ${(end - start)/1000.0}s")
  }

  private def generateDataPoint[Env <: Vectorizable with Environment[Action, Agent], Agent <: Vectorizable, Action <: Vectorizable]
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
