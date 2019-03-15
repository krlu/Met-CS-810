package org.bu


/**
  * Consists of miscellaneous/generic utility functions and type aliases
  * These usually don't belong to a particular object or class
  */
package object met810 {
  type Turn = Int
  type WinnerId = Int
  type NNVector = _root_.neuroflow.core.Network.Vector[Double]

  def choose[A](list: List[A]): A = choose(list.iterator)
  def choose[A](it: Iterator[A]): A =
    it.zip(Iterator.iterate(1)(_ + 1)).reduceLeft((row, col) =>
      if (util.Random.nextInt(col._2) == 0) col else row
    )._1

  def permutationsFromNBuckets[T](input: List[List[T]]): List[List[T]] = {
    if(input.isEmpty) input
    else if(input.size == 1) input.head.map(List(_))
    else {
      input.head.flatMap { i =>
        val restOfPerms: List[List[T]] = permutationsFromNBuckets(input.tail)
        restOfPerms.map { subList =>
          i :: subList
        }
      }
    }
  }

  def permutationsWithRepetitions[T](input: List[T], n: Int) : List[List[T]] = {
    require(input.nonEmpty && n > 0)
    n match {
      case 1 => for (el <- input) yield List(el)
      case _ => for (el <- input; perm <- permutationsWithRepetitions(input, n - 1)) yield el :: perm
    }
  }

  def possiblePositions(numRows: Int, numCols: Int, positionDim: Int): List[List[Int]] =
    permutationsWithRepetitions((0 until Math.max(numCols, numRows)).toList, positionDim)

  def applyNoise(pos: (Int, Int), positionRadius: Int, minFactor: Double): List[(Double, (Int, Int))] = {
    val (x, y) = pos
    for{
      xDelta <- -positionRadius to positionRadius
      yDelta <- -positionRadius to positionRadius
    } yield (1.0, (x + xDelta, y + yDelta))
  }.toList
}
