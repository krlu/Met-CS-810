package org.bu

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
}
