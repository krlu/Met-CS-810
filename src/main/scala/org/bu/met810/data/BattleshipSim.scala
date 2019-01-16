package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.battleshipassets.{Board, Move, Player}
import org.bu.met810.choose

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
      board.copy(p2 = opposingPlayer.removePiece(move.pos))
    else
      board.copy(p1 = opposingPlayer.removePiece(move.pos))
    Some(prevBoard, move, board)
  }
}

object BattleshipSim{
  def randomInitialization(width: Int, height: Int, numPieces: Int,
                           model1: PlayerModel[Board, Player, Move],
                           model2: PlayerModel[Board, Player, Move]): BattleshipSim = {
    val p1 = initPlayer(width, height, numPieces)
    val p2 = initPlayer(width, height, numPieces)
    val initialBoard = Board(p1, p2, width, height)
    new BattleshipSim(initialBoard, model1, model2)
  }

  private def initPlayer(width: Int, height: Int, numPieces:Int): Player = {
    var openPositions = {for{
      x <- 0 until width
      y <- 0 until height
    } yield (x,y)}.toList
    val piecePositions = (0 until numPieces).map{ _ =>
      val pos = choose(openPositions)
      openPositions = openPositions.filter(_ != pos)
      pos
    }
    Player(piecePositions.toList)
  }
}
