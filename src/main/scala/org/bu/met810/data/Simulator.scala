package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

class Simulator(initialBoard: Board, model1: PlayerModel[Board, Player, Move],
                model2: PlayerModel[Board, Player, Move], var turn: Int = 0){

  private val P1TURN = 0
  private val P2TURN = 1

  private var board: Board = initialBoard
  private var winner: Option[Player] = None

  def runSimulator(): Option[(Board, Move, Board)] = (board.p1, board.p2) match {
    case (p1, p2) if p1.position == p2.position =>
      winner = Some(board.p2)
      None
    case (p1, _) if p1.position == (initialBoard.length - 1, initialBoard.width - 1) =>
      winner = Some(board.p1)
      None
    case _ =>
      val move = if(turn == P1TURN) model1.selectMove(board.p1.id, board) else model2.selectMove(board.p2.id, board)
      val oldBoard = board
      board = updateBoard(turn, move)
      turn = if (turn == P1TURN) P2TURN else P1TURN
      Some(oldBoard, move, board)
  }

  def runFullGame(): Player ={
    while(winner.isEmpty)
      runSimulator()
    winner.orNull
  }

  private def updateBoard(turn: Int, move: Move): Board = {
    val player: Player = if(board.p1.id == turn) board.p1 else board.p2
    val (x,y) = player.position
    val updatedPlayer = player match {
      case c: Cop => c.copy(position = move(x,y))
      case r: Robber => r.copy(position = move(x,y))
    }
    if(board.p1.id == turn) board.copy(p1 = updatedPlayer) else board.copy(p2 = updatedPlayer)
  }

  def getWinner: Option[Player] = winner
  def isGameOver: Boolean = winner.nonEmpty
}

object Simulator{
  def apply(board: Board, p1: PlayerModel[Board, Player, Move], p2: PlayerModel[Board, Player, Move]): Simulator =
    new Simulator(board, p1,p2)
}
