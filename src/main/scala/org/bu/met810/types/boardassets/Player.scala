package org.bu.met810.types.boardassets

import org.bu.met810.types.NNAsset
import org.bu.met810.types.moves._

abstract class Player(val position: (Int, Int)) extends NNAsset{
  val id: Int = 0
  val moves: Set[Move]
  val toVector = Seq(position._1.toDouble, position._2.toDouble)
}

case class Robber(override val position: (Int, Int), override val id: Int = 0) extends Player(position){
  override val moves: Set[Move] = Set(Up, Down, Left, Right)
}

case class Cop(override val position: (Int, Int), override val id: Int = 1) extends Player(position){
  override val moves: Set[Move] = Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight)
}


