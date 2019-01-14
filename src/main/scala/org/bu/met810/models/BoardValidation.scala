package org.bu.met810.models

import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}
import org.bu.met810._

trait BoardValidation {

  /**
    * Filters out moves so player doesn't wander off the board
    * @param player - current player making move
    * @param board - game state
    * @return - List of valid moves
    */
  def validMoves(player: Player, board: Board): List[Move] = player.moves.filter{ m: Move =>
    validPosition(m(player.position._1, player.position._2), board)
  }

  /**
    * @param pos - current position
    * @param board - game state
    * @return true if and only if position is within the board boundaries, assumes rectangular board (for now)
    */
  def validPosition(pos: (Int, Int), board: Board): Boolean = {
    val (x1, y1) = pos
    x1 >= 0 && x1 < board.width && y1 >= 0 && y1 < board.length
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
      choose(applyNoise(otherPlayer.position, positionRadius = 1, minFactor = 0.5)
        .filter(p => validPosition(p._2, board) && p._2 != player.position))._2
      else otherPlayer.position
    }.toList
    (player, possiblePositions)
  }
}
