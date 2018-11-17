package org.bu.met810.models.inference

import org.bu.met810.types.boardassets.{Board, Cop, Robber}
import org.bu.met810.types.moves.{Down, Left, Move, Right, SkipDown, SkipLeft, SkipRight, SkipUp, Up}
import org.bu.met810.{Turn, WinnerId}

import scala.io.Source

trait Learner {

  val boardVectorDim = 6
  val moveVectorDim = 2

  def setupTrainingData(fileName: String): Seq[(Board, Move, Turn, WinnerId)] ={
    var trainingData = Seq.empty[(Board, Move, Turn, WinnerId)]
    val bufferedSource = Source.fromFile(fileName)
    for (line <- bufferedSource.getLines){
      val cols = line.split(",").map(_.trim.toDouble.toInt).toList
      val board = vectorToBoard(cols.dropRight(moveVectorDim))
      val move = vectorToMove(cols.drop(boardVectorDim).dropRight(2))
      val turn = cols(cols.size - 2)
      val winner = cols.last
      trainingData = trainingData :+ (board, move, turn, winner)
    }
    bufferedSource.close
    trainingData
  }
  def vectorToBoard(vector: Seq[Int]): Board = {
    val p1 = Robber((vector.head, vector(1)))
    val p2 = Cop((vector(2), vector(3)))
    Board(p1, p2, vector(4), vector(5), Seq())
  }
  def vectorToMove(vector: Seq[Int]): Move =
    Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight).find(_.toVector == vector.map(_.ceil.toInt)) match {
      case Some(move) => move
      case None => throw new NoSuchElementException(s"unable to find move with vector ${vector.map(_.ceil.toInt)}!")
    }
}
