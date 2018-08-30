package org.bu.met810.data

import org.bu.met810.model.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

class Simulator(board: Board, p1: PlayerModel[Board, Move], p2: PlayerModel[Board, Move]){
  def initializeBoard(): Unit = ???
  def getValidMovesForPlayer(player: Player): Set[Move] = ???
  def updateBoard(): Unit = ???

  def generateTrainingData(): (Seq[Double], Seq[Double]) = ???
}

object Simulator{
  def main(args: Array[String]): Unit = {
    val s = new Simulator(null, new RandomMoveModel(), new RandomMoveModel())
  }
}
