package org.bu.met810.types.copsandrobbersassets

import org.bu.met810.types.{Action, Vectorizable}

abstract class Move() extends Action with ((Int, Int) => (Int, Int))with Vectorizable

object Up extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+1)
  override val id: Int = 1
  override val toVector: Seq[Double] = Seq(0, 1)
  override def toString(): String = "Up"
}
object Down extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-1)
  override val id: Int = 2
  override val toVector: Seq[Double] = Seq(0, -1)
  override def toString(): String = "Down"
}
object Left extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-1, y)
  override val id: Int = 3
  override val toVector: Seq[Double] = Seq(-1, 0)
  override def toString(): String = "Left"
}
object Right extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+1, y)
  override val id: Int = 4
  override val toVector: Seq[Double] = Seq(1, 0)
  override def toString(): String = "Right"
}
object SkipUp extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+2)
  override val id: Int = 5
  override val toVector: Seq[Double] = Seq(0, 2)
  override def toString(): String = "SkipUp"
}
object SkipDown extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-2)
  override val id: Int = 6
  override val toVector: Seq[Double] = Seq(0, -2)
  override def toString(): String = "SkipDown"
}
object SkipLeft extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-2, y)
  override val id: Int = 7
  override val toVector: Seq[Double] = Seq(-2, 0)
  override def toString(): String = "SkipLeft"

}
object SkipRight extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+2, y)
  override val id: Int = 8
  override val toVector: Seq[Double] = Seq(2, 0)
  override def toString(): String = "SkipRight"

}
