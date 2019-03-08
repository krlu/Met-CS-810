package org.bu.met810.data

import org.bu.met810.choose
import org.bu.met810.models.PlayerModel
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.battleshipassets.{Board, Move, Player}

class BattleshipSim(initialBoard: Board,
                    val model1: PlayerModel[Board, Player, Move],
                    val model2: PlayerModel[Board, Player, Move],
                    var turn: Int = 0,
                    val shouldApplyNoise: Boolean = false) extends Simulator[Board, Player, Move]{

  override protected var board: Board = initialBoard

  override def transition(p1: Player, p2: Player, move: Move, env: Board): Board = {
    val newMovesMade = p1.movesMade.updated(move.pos, 1)
    val newPosDestroyed =
      if(p2.positions.contains(move.pos)) p2.positionsDestroyed :+ move.pos
      else p2.positionsDestroyed
    val (newP1, newP2) =
      if(turn == P1TURN)
        (p1.copy(movesMade = newMovesMade), p2.copy(positionsDestroyed = newPosDestroyed))
      else
        (p2.copy(positionsDestroyed = newPosDestroyed), p1.copy(movesMade = newMovesMade))
    board.copy(p1 = newP1, p2 = newP2)
  }

  override def determineWinner(board: Board): Option[Player] = {
    if(board.p2.isDestroyed) Some(board.p1)
    else if(board.p2.isDestroyed) Some(board.p1)
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
  def randomInitialization(model1: PlayerModel[Board, Player, Move] = new RandomMoveModel[Board, Player, Move](null),
                           model2: PlayerModel[Board, Player, Move] = new RandomMoveModel[Board, Player, Move](null),
                           envSize: Int, shouldApplyNoise: Boolean = false): BattleshipSim = {
    val numPieces = envSize/2
    val pieces = (0 until numPieces).map(_ => choose(pieceLengths)).toList
    val width = envSize
    val height = envSize
    val p1 = initPlayer(width, height, pieces, 1)
    val p2 = initPlayer(width, height, pieces, 2)
    val initialBoard = Board(p1, p2, width, height)
    new BattleshipSim(initialBoard, model1, model2)
  }

  private def initPlayer(width: Int, height: Int, pieces: List[Int], id: Int): Player = {
    def getAllPositions = {for{
      x <- 0 until width
      y <- 0 until height
    } yield (x,y)}.toList
    var openPositions = getAllPositions
    val positions: Seq[(Int, Int)] = pieces.flatMap{ pieceLength =>
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
    Player(positions.toList, id, getAllPositions.map(p => p -> 0).toMap)
  }
}
