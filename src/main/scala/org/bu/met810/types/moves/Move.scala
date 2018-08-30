package org.bu.met810.types.moves

abstract class Move() extends ((Int, Int) => (Int, Int)){
  val id: Int
}

object Up extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+1)
  override val id: Int = 1
}
object Down extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-1)
  override val id: Int = 2
}
object Left extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-1, y)
  override val id: Int = 3
}
object Right extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+1, y)
  override val id: Int = 4
}

object SkipUp extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+2)
  override val id: Int = 5
}
object SkipDown extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-2)
  override val id: Int = 6
}
object SkipLeft extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-2, y)
  override val id: Int = 7
}
object SkipRight extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+2, y)
  override val id: Int = 8
}
