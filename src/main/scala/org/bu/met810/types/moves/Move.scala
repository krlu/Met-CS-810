package org.bu.met810.types.moves

abstract class Move() extends ((Int, Int) => (Int, Int))

object Up extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+1) }
object Down extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-1) }
object Left extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-1, y) }
object Right extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+1, y) }

object SkipUp extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+2) }
object SkipDown extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-2) }
object SkipLeft extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-2, y) }
object SkipRight extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+2, y) }
