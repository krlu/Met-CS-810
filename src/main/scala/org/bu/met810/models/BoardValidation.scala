package org.bu.met810.models

import org.bu.met810.applyNoise
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

trait BoardValidation {
  def validMoves(player: Player, board: Board): List[Move] = player.moves.filter{ m: Move =>
    val (x1, y1) = m(player.position._1, player.position._2)
    x1 >= 0 && x1 < board.width && y1 >= 0 && y1 < board.length
  }

  def validPosition(pos: (Int, Int), board: Board): Boolean = {
    val (x1, y1) = pos
    x1 >= 0 && x1 < board.width && y1 >= 0 && y1 < board.length
  }

  def getPlayerData(playerId: Int, board: Board): (Player, List[List[(Double, (Int, Int))]]) ={
    val players = Seq(board.p1, board.p2)
    val player: Player = players.find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    val otherPlayers = players.filter(_.id != playerId)
    val possiblePositions: List[List[(Double, (Int, Int))]] = otherPlayers.map{ otherPlayer =>
      applyNoise(otherPlayer.position, positionRadius = 1, minFactor = 0.5)
        .filter(p => validPosition(p._2, board) && p._2 != player.position)
    }.toList
    (player, possiblePositions)
  }
}
