package org.bu.simmba.types.copsandrobbersassets

import org.bu.simmba.types.{Agent, Vectorizable}

/**
  * @param position - x y coordinate of player on manhattan board
  */
abstract class Player(val position: (Int, Int)) extends Vectorizable with Agent{
  val id: Int = 0
  val moves: List[Move] = List.empty[Move]
  val state: List[Int] = List(position._1, position._2)
  val toVector: Seq[Double] = state.map(_.toDouble)
}
case class Robber(override val position: (Int, Int), override val id: Int = 0) extends Player(position)
case class Cop(override val position: (Int, Int), override val id: Int = 1) extends Player(position)

