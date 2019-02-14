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

  override var board: Board = initialBoard

  /**
    * Performs one turn update in the simulation
    * @return
    */
  override def runSimulator(): Option[(Board, Move, Board)] = {
    val model = if(turn == 0) model1 else model2
    val opposingPlayer = if(turn == 0) board.p2 else board.p1
    val player = if(turn == 0) board.p1 else board.p2
    val move = model.selectMove(player, board)
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

    var openPositions = {for{
      x <- 0 until width
      y <- 0 until height
    } yield (x,y)}.toList
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
    Player(positions.toList, id)
  }
}
