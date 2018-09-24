package org.bu.met810.model.bayes

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
  */
class PlayerModelLearner(numRows: Int, numCols: Int){
  val allMoves = List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
  val modelParams = ModelParameters()
  val initialMoveParams: Seq[AtomicDirichlet] = {
    for{
      x1 <- 0 until numRows
      y1 <- 0 until numCols
      x2 <- 0 until numRows
      y2 <- 0 until numCols
      if x1 != x2 || y1 != y2
    } yield Dirichlet(Array.fill(allMoves.size)(1.0):_*)(s"${List(x1,y1,x2,y2).mkString("_")}_move", modelParams)
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

  def generateTrial(positions: List[(Int, Int)], move: Move): Unit = {
    val posVector = positions.flatMap{case (x,y) => List(x,y)}
    val params = modelParams.getElementByReference(posVector.mkString("_") + "_move").asInstanceOf[AtomicDirichlet]
    val moveDist = Select(params, allMoves:_*)
    moveDist.observe(move)
  }
}

object PlayerModelLearner{
  def main(args: Array[String]): Unit = {
    val pml = new PlayerModelLearner(10, 10)
    val data = List.fill(10){
      (List((1,1), (2,2)), Up)
    }
    val string = pml.train(data)
    println(string)
  }
}
