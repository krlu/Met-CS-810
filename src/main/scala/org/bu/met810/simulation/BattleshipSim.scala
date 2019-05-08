package org.bu.met810.simulation

import org.bu.met810.choose
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.battleshipassets.{Board, Move, Player}

class BattleshipSim(initialBoard: Board,
                    val model1: PlayerModel[Board, Player, Move],
                    val model2: PlayerModel[Board, Player, Move], val firstMove: Int = 0) extends Simulator[Board, Player, Move]{

  override protected var turn: Int = firstMove
  override protected var board: Board = initialBoard
  private val DESTROYED_ID = 0

  override def transition(p1: Player, move: Move, env: Board): Board = {
    val p2 = if (turn == P1TURN) board.p2 else board.p1
    val p2PosStatus = p2.positions.find(e => e._1 == move.pos).get._2
    val newMovesMade =  p1.movesMade.map{case (pos, status) => if(pos == move.pos) pos -> p2PosStatus else pos -> status}
    val newPosDestroyed = p2.positions.map{case (pos, status) => if(pos == move.pos) pos -> DESTROYED_ID else pos -> status}
    val (newP1, newP2) = (p1.copy(movesMade = newMovesMade), p2.copy(positions = newPosDestroyed))
    if(turn == P1TURN) board.copy(p1 = newP1, p2 = newP2)
    else board.copy(p1 = newP2, p2 = newP1)
  }

  override def determineWinner(board: Board): Option[Player] = {
    if(board.p2.isDestroyed) Some(board.p1)
    else if(board.p1.isDestroyed) Some(board.p2)
    else None
  }

}

object BattleshipSim extends SimBuilder[Board, Player, Move]{

  private val NORTHING = 0
  private val EASTING = 1
  val pieceLengths: List[Int] = (2 to 4).toList

  /**
    * @param model1 - reasoning model for player 1, default Random selector
    * @param model2 - reasoning model for player 2, default Random selector
    * @param envSize - width and height of board, assumes square board
    * @param shouldApplyNoise - whether to apply noise, default false
    * @return BattleshipSim
    */
  def randomInitialization(model1: PlayerModel[Board, Player, Move],
                           model2: PlayerModel[Board, Player, Move],
                           envSize: Int, shouldApplyNoise: Boolean = false, firstMove: Int): BattleshipSim = {
    val numPieces = 2
    val pieces = (0 until numPieces).map(_ => choose(pieceLengths)).toList
    val width = envSize
    val height = envSize
    val p1 = initPlayer(width, height, pieces, 0)
    val p2 = initPlayer(width, height, pieces, 1)
    val initialBoard = Board(p1, p2, width, height)
    new BattleshipSim(initialBoard, model1, model2, firstMove)
  }

  private def initPlayer(width: Int, height: Int, pieces: List[Int], id: Int): Player = {
    val allPositions = {for{
      x <- 0 until width
      y <- 0 until height
    } yield (x,y)}.toList
    var openPositions = allPositions
    val takenPositions = pieces.flatMap{ pieceLength =>
      val orientation = choose(List(NORTHING, EASTING))
      val validPositions = openPositions.filter{ case (x,y) =>
        if(orientation == EASTING) (0 until pieceLength).forall(i => openPositions.contains((x + i, y)))
        else if(orientation == NORTHING) (0 until pieceLength).forall(i => openPositions.contains((x, y + i)))
        else throw new IllegalStateException(s"orientation should be northing (0) or easting (1), but was $orientation")
      }
      val pos = choose(validPositions)
      val positionsForPiece = (0 until pieceLength).map{ i =>
        val (x,y) = pos
        if(orientation == EASTING) (x + i, y)
        else if(orientation == NORTHING) (x, y + i)
        else throw new IllegalStateException(s"orientation should be northing (0) or easting (1), but was $orientation")
      }
      openPositions = openPositions.filter(!positionsForPiece.contains(_))
      positionsForPiece
    }
    val initPositions = allPositions.map(p => if(takenPositions.contains(p)) p -> 1 else p -> 0)
    Player(initPositions, id, allPositions.map(p => p -> -1))
  }
}
