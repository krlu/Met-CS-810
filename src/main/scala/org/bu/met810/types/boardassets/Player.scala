package org.bu.met810.types.boardassets

import org.bu.met810.types.moves._

abstract class Player(position: (Int, Int)){
  val moves: Set[Move]
}

class Cop(position: (Int, Int)) extends Player(position){
  override val moves: Set[Move] = Set(Up, Down, Left, Right,SkipUp, SkipDown, SkipLeft, SkipRight)
}

class Robber(position: (Int, Int)) extends Player(position){
  override val moves: Set[Move] = Set(Up, Down, Left, Right)
}
