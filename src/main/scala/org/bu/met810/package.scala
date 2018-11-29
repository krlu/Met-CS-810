package org.bu


/**
  * Consists of miscellaneous/generic utility functions and type aliases
  * These usually don't belong to a particular object or class
  */
package object met810 {
  type Turn = Int
  type WinnerId = Int
  type NNVector = _root_.neuroflow.core.Network.Vector[Double]

  def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1

  def permutationsWithRepetitions[T](input : List[T], n : Int) : List[List[T]] = {
    require(input.nonEmpty && n > 0)
    n match {
      case 1 => for (el <- input) yield List(el)
      case _ => for (el <- input; perm <- permutationsWithRepetitions(input, n - 1)) yield el :: perm
    }
  }

  def possiblePositions(numRows: Int, numCols: Int): List[(Int, Int)] = {for{
      x <- 0 until numCols
      y <- 0 until numRows
    }yield(x,y)
  }.toList

  def possibleDifferentPositions(numRows: Int, numCols: Int, numPlayers: Int): List[List[(Int, Int)]] = {
    val pos = possiblePositions(numRows, numRows)
    pos.combinations(numPlayers).toList
  }
}
