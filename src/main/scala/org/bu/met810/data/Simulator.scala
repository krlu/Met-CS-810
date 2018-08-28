package org.bu.met810.data

import org.bu.met810.model.PlayerModel
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

class Simulator(board: Board, p1: PlayerModel, p2: PlayerModel){
  def initializeBoard(): Unit = ???
  def getValidMovesForPlayer(player: Player): Set[Move] = ???
  def updateBoard(): Unit = ???
}
