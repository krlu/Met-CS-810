package org.bu.met810.types.copsandrobbersassets

import org.bu.met810.types.{Agent, Vectorizable}

abstract class Player(val positions: List[(Int, Int)]) extends Vectorizable with Agent{
  val id: Int = 0
  val moves: List[Move] = List.empty[Move]
  val toVector: Seq[Double] = Seq(positions.head._1.toDouble, positions.head._2.toDouble)
}

case class Robber(position: (Int, Int), override val id: Int = 0) extends Player(List(position)){
  override val moves: List[Move] = List(Up, Down, Left, Right)
}

case class Cop(position: (Int, Int), override val id: Int = 1) extends Player(List(position)){
  override val moves: List[Move] = List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
}


