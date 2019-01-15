package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.battleshipassets.{Board, Move, Player}

class BattleshipSim(initialBoard: Board,
                    val model1: PlayerModel[Board, Player, Move],
                    val model2: PlayerModel[Board, Player, Move])extends Simulator[Board, Player, Move]{

  override var board: Board = initialBoard
  override var turn: Int = 0
  override val shouldApplyNoise: Boolean = false

  /**
    * Performs one turn update in the simulation
    * @return
    */
  override def runSimulator(): Option[(Board, Move, Board)] = {
    val model = if(turn == 0) model1 else model2
    val opposingPlayer = if(turn == 0) board.p2 else board.p1
    val move = model.selectMove(turn, board)
    val prevBoard = board
    if(turn == 0)
      board.copy(p2 =opposingPlayer.removePiece(move.pos))
    else
      board.copy(p1 =opposingPlayer.removePiece(move.pos))
    Some(prevBoard, move, board)
  }
}
