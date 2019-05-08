package org.bu.simmba.models

import org.bu.simmba.types.copsandrobbersassets.{Board, Move, Player}
import org.bu.simmba._

trait BoardValidation {

  /**
    * Filters out moves so player doesn't wander off the board
    * @param player - current player making move
    * @param dims - dimensions of board
    * @return - List of valid moves
    */
  def validMoves(player: Player, dims: (Int, Int)): List[Move] = player.moves.filter{ m: Move =>
    val (x,y) = player.positions.head
    validPosition(m(x,y), dims)
  }

  /**
    * @param pos - current position
    * @param dims - dimensions of board
    * @return true if and only if position is within the board boundaries, assumes rectangular board (for now)
    */
  def validPosition(pos: (Int, Int), dims: (Int, Int)): Boolean = {
    val (width, length) = dims
    val (x1, y1) = pos
    x1 >= 0 && x1 < width && y1 >= 0 && y1 < length
  }


  /**
    * @param playerId - id of current player
    * @param board - current game state
    * @param useNoise - whether to apply noise
    * @return - (player object corresponding to id, semi-random position for all other players)
    */
  def getPlayerData(playerId: Int, board: Board, useNoise: Boolean = false): (Player, List[(Int, Int)]) ={
    val players = Seq(board.p1, board.p2)
    val player: Player = players.find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    val otherPlayers = players.filter(_.id != playerId)
    val possiblePositions: List[(Int, Int)] = otherPlayers.map{ otherPlayer =>
      if(useNoise)
      choose(applyNoise(otherPlayer.positions.head, positionRadius = 1, minFactor = 0.5)
        .filter(p => validPosition(p._2, (board.width, board.length)) && p._2 != player.positions.head))._2
      else otherPlayer.positions.head
    }.toList
    (player, possiblePositions)
  }
}
