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
    val newMovesMade: Map[Int, Set[Move]] = board.movesMade ++ Map(turn -> Set(move))
    board =
      if(turn == 0)
        board.copy(p2 = opposingPlayer.removePiece(move.pos), movesMade = newMovesMade)
      else
        board.copy(p1 = opposingPlayer.removePiece(move.pos), movesMade = newMovesMade)
    Some(prevBoard, move, board)
  }
}

object BattleshipSim {

  private val NORTHING = 0
  private val EASTING = 0

  def randomInitialization(width: Int, height: Int, numPieces: Int,
                           model1: PlayerModel[Board, Player, Move],
                           model2: PlayerModel[Board, Player, Move]): BattleshipSim = {
    val p1 = initPlayer(width, height, numPieces, 1)
    val p2 = initPlayer(width, height, numPieces, 2)
    val initialBoard = Board(p1, p2, width, height)
    new BattleshipSim(initialBoard, model1, model2)
  }

  private def initPlayer(width: Int, height: Int, numPieces:Int, id: Int): Player = {
    val pieceLengths = (1 to 4).toList
    var openPositions = {for{
      x <- 0 until width
      y <- 0 until height
    } yield (x,y)}.toList
    val positions: Seq[(Int, Int)] = (0 until numPieces).flatMap{ _ =>
      val pieceLength = choose(pieceLengths)
      val orientation = choose(List(NORTHING, EASTING))
      val validPositions = openPositions.filter{ case (x,y) =>
        if(orientation == EASTING) (0 until pieceLength).forall(i => openPositions.contains((i, y)))
        else if(orientation == NORTHING) (0 until pieceLength).forall(i => openPositions.contains((x, i)))
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
    Player(positions.toList, id)
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
