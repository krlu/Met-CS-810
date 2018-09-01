package org.bu.met810.types.boardassets

import org.bu.met810.types.NNAsset
import org.bu.met810.types.moves._

case class Player(position: (Int, Int)) extends NNAsset{
  val id: Int = 0
  val moves: Set[Move] = Set.empty[Move]
  val toVector = Seq(position._1.toDouble, position._2.toDouble)
}

class Cop(position: (Int, Int), override val id: Int = 0) extends Player(position){
  override val moves: Set[Move] = Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight)
}

class Robber(position: (Int, Int), override val id: Int = 1) extends Player(position){
  override val moves: Set[Move] = Set(Up, Down, Left, Right)
}
