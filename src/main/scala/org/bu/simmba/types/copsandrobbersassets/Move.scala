package org.bu.simmba.types.copsandrobbersassets

import org.bu.simmba.types.Vectorizable

abstract class Move() extends ((Int, Int) => (Int, Int)) with Vectorizable
object Move{
  val possibleMoves = List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
  val copMoves =  List(Up, Down, Left, Right, SkipUp, SkipDown, SkipLeft, SkipRight)
  val robberMoves = List(Up, Down, Left, Right)
}

object Up extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+1)
  override val toVector: Seq[Double] = Seq(0, 1)
  override def toString(): String = "Up"
}
object Down extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-1)
  override val toVector: Seq[Double] = Seq(0, -1)
  override def toString(): String = "Down"
}
object Left extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-1, y)
  override val toVector: Seq[Double] = Seq(-1, 0)
  override def toString(): String = "Left"
}
object Right extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+1, y)
  override val toVector: Seq[Double] = Seq(1, 0)
  override def toString(): String = "Right"
}
object SkipUp extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+2)
  override val toVector: Seq[Double] = Seq(0, 2)
  override def toString(): String = "SkipUp"
}
object SkipDown extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-2)
  override val toVector: Seq[Double] = Seq(0, -2)
  override def toString(): String = "SkipDown"
}
object SkipLeft extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-2, y)
  override val toVector: Seq[Double] = Seq(-2, 0)
  override def toString(): String = "SkipLeft"

}
object SkipRight extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+2, y)
  override val toVector: Seq[Double] = Seq(2, 0)
  override def toString(): String = "SkipRight"

}
