package org.bu.met810.data

import org.bu.met810.choose
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
    val p1 = initPlayer(width, height, numPieces, 1)
    val p2 = initPlayer(width, height, numPieces, 2)
    val initialBoard = Board(p1, p2, width, height)
    new BattleshipSim(initialBoard, model1, model2)
  }

  private def initPlayer(width: Int, height: Int, numPieces:Int, id: Int): Player = {
    var openPositions = {for{
      x <- 0 until width
      y <- 0 until height
    } yield (x,y)}.toList
    val piecePositions = (0 until numPieces).map{ _ =>
      val pos = choose(openPositions)
      openPositions = openPositions.filter(_ != pos)
      pos
    }
    Player(piecePositions.toList, id)
  }

  def runBatch(p1Model: PlayerModel[Board, Player, Move],
               p2Model: PlayerModel[Board, Player, Move],
               numTrials: Int = 1000, boardSize: Int = 4,
               numPieces: Int): (Int, Int) = {
    val start = System.currentTimeMillis()
    val winners: Seq[Player] = {
      for(_ <- 1 to numTrials) yield {
        val sim = randomInitialization(boardSize, boardSize,numPieces, p1Model, p2Model)
        sim.runFullGame()
      }
    }.flatten
    val p1Wins = winners.filter(p => p.id == 1)
    val p2Wins = winners.filter(p => p.id == 2)
    val end = System.currentTimeMillis()
    println(s"runtime: ${(end - start).toDouble/1000}")
    (p1Wins.size, p2Wins.size)
  }
}
