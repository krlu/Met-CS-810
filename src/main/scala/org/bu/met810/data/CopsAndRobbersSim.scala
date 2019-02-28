package org.bu.met810.data

import org.bu.met810.{applyNoise, choose}
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.copsandrobbersassets.{Move, _}

class CopsAndRobbersSim(initialBoard: Board,
                        val model1: PlayerModel[Board, Player, Move],
                        val model2: PlayerModel[Board, Player, Move],
                        var turn: Int = 0,
                        val shouldApplyNoise: Boolean = false) extends Simulator[Board, Player, Move]{

  private val P1TURN = 0
  private val P2TURN = 1
  override var board: Board = initialBoard

  def runSimulator(): Option[(Board, Move, Board)] = (board.p1, board.p2) match {
    case (p1, p2) if p1.positions.head == p2.positions.head =>
      winner = Some(board.p2)
      None
    case (p1, _) if p1.positions.head == (initialBoard.length - 1, initialBoard.width - 1) =>
      winner = Some(board.p1)
      None
    case _ =>
      val move = if (turn == P1TURN) model1.selectMove(board.p1, board) else model2.selectMove(board.p2, board)
      val oldBoard =
        if(shouldApplyNoise){ // TODO: noise only gets applied to P2 position for now!!
           val (_, pos) = choose(applyNoise(board.p2.positions.head, 1, 0.5)) // TODO: noise arguments hard coded for now!!
           val newP2 = board.p2.asInstanceOf[Cop].copy(pos)
          board.copy(p2 = newP2)
        }
        else board
      board = updateBoard(turn, move)
      turn = if (turn == P1TURN) P2TURN else P1TURN
      Some(oldBoard, move, board)
  }

  private def updateBoard(turn: Int, move: Move): Board = {
    val player: Player = if (board.p1.id == turn) board.p1 else board.p2
    val (x, y) = player.positions.head
    val updatedPlayer = player match {
      case c: Cop => c.copy(position = move(x, y))
      case r: Robber => r.copy(position = move(x, y))
    }
    if (board.p1.id == turn) board.copy(p1 = updatedPlayer)
    else board.copy(p2 = updatedPlayer)
  }
}

object CopsAndRobbersSim extends SimBuilder[Board, Player, Move]{
  def apply(board: Board,
            p1: PlayerModel[Board, Player, Move],
            p2: PlayerModel[Board, Player, Move],
            shouldApplyNoise: Boolean = false): CopsAndRobbersSim = new CopsAndRobbersSim(board, p1,p2)

  def randomInitialization(robberModel: PlayerModel[Board, Player, Move],
                           copModel: PlayerModel[Board, Player, Move],
                           boardSize: Int = 4, shouldApplyNoise: Boolean): CopsAndRobbersSim = {
    val positions = 0 until boardSize
    val rX = choose(positions.iterator)
    val rY = choose(positions.iterator)
    val cX = choose(positions.filter(_ != rX).iterator)
    val cY = choose(positions.filter(_ != rY).iterator)
    val board = Board(Robber((rX, rY)), Cop((cX, cY)), boardSize, boardSize, Seq.empty[Building])
    CopsAndRobbersSim(board, robberModel, copModel, shouldApplyNoise)
  }
}
