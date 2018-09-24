package org.bu.met810.model.bayes

import java.io.PrintWriter

import argonaut._
import Argonaut._
import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.language.Select
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810.types.moves._


/** Given the dimensions of the board we build up our initial distributions
  * @param numRows - number of rows on the board
  * @param numCols - number of columns on the board
  * @param numPlayers - number of active players
  */
class PlayerModelLearner(numRows: Int, numCols: Int, numPlayers: Int = 2){
  val allMoves = List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
  val modelParams = ModelParameters()

  private def possiblePositions(): List[(Int, Int)] =
    {for{
      x <- 0 until numCols
      y <- 0 until numRows
    }yield(x,y)}.toList

  val initialMoveParams: Seq[AtomicDirichlet] =
    permutationsWithRepetitions(possiblePositions(), numPlayers).map{ positions =>
      val name = s"${positions.flatMap{case(a,b) => List(a,b)}.mkString("_")}_move"
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
    val params = modelParams.getElementByReference(posVector.mkString("_") + "_move").asInstanceOf[AtomicDirichlet]
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
    val numRows = 2
    val numCols = 1
    val numPlayers = 2
    val pml = new PlayerModelLearner(numRows, numCols, numPlayers)
    val data: List[(List[(Int, Int)], Move)] = List.fill(10){
      val x1 = (Math.random() * numCols).toInt
      val y1 = (Math.random() * numRows).toInt
      val x2 = (Math.random() * numCols).toInt
      val y2 = (Math.random() * numRows).toInt
      (List((x1,y1), (x2,y2)), Up)
    }
    val paramsString = pml.train(data)
    val pw = new PrintWriter("model.json")
    pw.println(paramsString)
    pw.close()
  }
}
