package org.bu.met810.data

import org.bu.met810.choose
import org.bu.met810.models.{PlayerModel, RandomMoveModel}
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

class Simulator(initialBoard: Board, model1: PlayerModel[Board, Player, Move],
                model2: PlayerModel[Board, Player, Move], var turn: Int = 0){

  private val P1TURN = 0
  private val P2TURN = 1

  private val MAX_COUNTER = 1000
  private var counter = 0
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

  def runFullGame(): Option[Player] ={
    while(winner.isEmpty && counter < MAX_COUNTER)
      runSimulator()
    winner
  }

  private def updateBoard(turn: Int, move: Move): Board = {
    val player: Player = if(board.p1.id == turn) board.p1 else board.p2
    val (x,y) = player.position
    val updatedPlayer = player match {
      case c: Cop => c.copy(position = move(x,y))
      case r: Robber => r.copy(position = move(x,y))
    }
    counter += 1
    if(board.p1.id == turn) board.copy(p1 = updatedPlayer) else board.copy(p2 = updatedPlayer)
  }

  def getWinner: Option[Player] = winner
  def isGameOver: Boolean = winner.nonEmpty
}

object Simulator{
  def apply(board: Board, p1: PlayerModel[Board, Player, Move], p2: PlayerModel[Board, Player, Move]): Simulator =
    new Simulator(board, p1,p2)

  /**
    * Runs a batch of experiments with num trials, returns number of times robber and cop won
    * @param robberModel - model for robber, default is random selector
    * @param copModel - model for cop, default is random selector
    * @param numTrials - Number of times we run simulator
    * @return (number of robber wins, number of cop wins)
    */
  def runBatch(robberModel: PlayerModel[Board, Player, Move] = RandomMoveModel(),
               copModel: PlayerModel[Board, Player, Move] = RandomMoveModel(),
               numTrials: Int = 1000, boardSize: Int = 4): (Int, Int) = {
    val positions = 0 until boardSize
    val start = System.currentTimeMillis()
    val winners: Seq[Player] = {
      for(_ <- 1 to numTrials) yield {
        val rX = choose(positions.iterator)
        val rY = choose(positions.iterator)
        val cX = choose(positions.filter(_ != rX).iterator)
        val cY = choose(positions.filter(_ != rY).iterator)
        val board = Board(Robber((rX, rY)), Cop((cX, cY)), boardSize, boardSize, Seq.empty[Building])
        val sim = Simulator(board, robberModel, copModel)
        sim.runFullGame()
      }
    }.flatten
    val robbers = winners.filter(p => p.isInstanceOf[Robber])
    val cops = winners.filter(p => p.isInstanceOf[Cop])
    val end = System.currentTimeMillis()
    println(s"runtime: ${(end - start).toDouble/1000}")
    (robbers.size, cops.size)
  }
}
