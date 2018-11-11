package org.bu.met810.model.bayes

import java.io.PrintWriter

import argonaut.Argonaut._
import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.language.Select
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810.{Turn, WinnerId}
import org.bu.met810.types.boardassets.{Board, Cop, Robber}
import org.bu.met810.types.moves.{Move, _}

import scala.io.Source


/** Given the dimensions of the board we build up our initial distributions
  * @param numRows - number of rows on the board
  * @param numCols - number of columns on the board
  * @param numPlayers - number of active players
  */
class PlayerModelLearner(numRows: Int, numCols: Int, numPlayers: Int = 2, playerId: Int = 0){
  val allMoves = List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
  val modelParams = ModelParameters()

  private def possiblePositions(): List[(Int, Int)] =
    {for{
      x <- 0 until numCols
      y <- 0 until numRows
    }yield(x,y)}.toList

  val initialMoveParams: Seq[AtomicDirichlet] =
    permutationsWithRepetitions(possiblePositions(), numPlayers).map{ positions =>
      val name = s"${playerId}_${positions.flatMap{case(a,b) => List(a,b)}.mkString("_")}_move"
      Dirichlet(Array.fill(allMoves.size)(1.0):_*)(name, modelParams)
    }

  def train(data: List[(List[(Int, Int)], Move)]): String = {
    data.foreach{ case(p, m) => generateTrial(p,m)}
    val alg = EMWithVE(2, modelParams)
    alg.start()
    val savedParams = modelParams.asJson.toString()
    alg.stop()
    alg.kill()
    savedParams
  }

  private def generateTrial(positions: List[(Int, Int)], move: Move): Unit = {
    val posVector = positions.flatMap{case (x,y) => List(x,y)}
    val params = modelParams.getElementByReference(s"${playerId}_${posVector.mkString("_")}_move").asInstanceOf[AtomicDirichlet]
    val moveDist = Select(params, allMoves:_*)
    moveDist.observe(move)
  }

  private def permutationsWithRepetitions[T](input : List[T], n : Int) : List[List[T]] = {
    require(input.nonEmpty && n > 0)
    n match {
      case 1 => for (el <- input) yield List(el)
      case _ => for (el <- input; perm <- permutationsWithRepetitions(input, n - 1)) yield el :: perm
    }
  }
}

object PlayerModelLearner{
  def main(args: Array[String]): Unit = {
    val numRows = 4
    val numCols = 4
    val playerId = 0
    val numPlayers = 2

    def trainForPlayer(playerId: Int): Unit ={
      val pml = new PlayerModelLearner(numRows, numCols, numPlayers, playerId)
      val data = setupTrainingData("training_data.csv")
      val p1Data = data.filter{ case (_, _, turn, winnerId) =>
        turn == playerId && winnerId == playerId
      }.map{ case (board, move, _, _) =>
        (List(board.p1.position, board.p2.position), move)
      }.toList
      println(p1Data.size)
      val paramsString = pml.train(p1Data)
      val pw = new PrintWriter(s"model_${playerId}_${numRows}by${numCols}_v3.json")
      pw.println(paramsString)
      pw.close()
    }
    val start = System.currentTimeMillis()
    trainForPlayer(playerId)
    val end = System.currentTimeMillis()
    println(s"Training time: ${(end - start)/1000.0}s")
  }

  val boardVectorDim = 6
  val moveVectorDim = 2

  private def setupTrainingData(fileName: String): Seq[(Board, Move, Turn, WinnerId)] ={
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
