package org.bu.met810.types.copsandrobbersassets

import org.bu.met810.types.{Agent, Vectorizable}

abstract class Player(val positions: List[(Int, Int)]) extends Vectorizable with Agent{
  val id: Int = 0
  val moves: List[Move] = List.empty[Move]
  val state: List[Int] = positions.flatMap(p => List(p._1, p._2))
  val toVector: Seq[Double] = state.map(_.toDouble)
}
case class Robber(position: (Int, Int), override val id: Int = 0) extends Player(List(position))
case class Cop(position: (Int, Int), override val id: Int = 1) extends Player(List(position))

