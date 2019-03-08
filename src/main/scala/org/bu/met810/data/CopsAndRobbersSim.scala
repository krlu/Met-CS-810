package org.bu.met810.data

import org.bu.met810.{applyNoise, choose}
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.copsandrobbersassets.{Move, _}

class CopsAndRobbersSim(initialBoard: Board,
                        val model1: PlayerModel[Board, Player, Move],
                        val model2: PlayerModel[Board, Player, Move],
                        var turn: Int = 0,
                        val shouldApplyNoise: Boolean = false) extends Simulator[Board, Player, Move]{

  override var board: Board = initialBoard

  override def transition(agent1: Player, agent2: Player, action: Move, env: Board): Board = {
    val (x, y) = agent1.positions.head
    val updatedPlayer = agent1 match {
      case c: Cop => c.copy(position = action(x, y))
      case r: Robber => r.copy(position = action(x, y))
    }
    if (board.p1.id == turn) board.copy(p1 = updatedPlayer)
    else board.copy(p2 = updatedPlayer)
  }

  override def determineWinner(board: Board): Option[Player] = (board.p1, board.p2) match {
    case (p1, p2) if p1.positions.head == p2.positions.head => Some(board.p2)
    case (p1, _) if p1.positions.head == (initialBoard.length - 1, initialBoard.width - 1) => Some(board.p1)
    case _ => None
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
