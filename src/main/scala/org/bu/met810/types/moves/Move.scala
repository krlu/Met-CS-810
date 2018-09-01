package org.bu.met810.types.moves

import org.bu.met810.types.NNAsset

abstract class Move() extends ((Int, Int) => (Int, Int)) with NNAsset

object Up extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+1)
  override val id: Int = 1
  override val toVector: Seq[Double] = Seq(0, 1)
}
object Down extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-1)
  override val id: Int = 2
  override val toVector: Seq[Double] = Seq(0, -1)
}
object Left extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-1, y)
  override val id: Int = 3
  override val toVector: Seq[Double] = Seq(-1, 0)
}
object Right extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+1, y)
  override val id: Int = 4
  override val toVector: Seq[Double] = Seq(1, 0)
}
object SkipUp extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y+2)
  override val id: Int = 5
  override val toVector: Seq[Double] = Seq(0, 2)
}
object SkipDown extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x, y-2)
  override val id: Int = 6
  override val toVector: Seq[Double] = Seq(0, -2)
}
object SkipLeft extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x-2, y)
  override val id: Int = 7
  override val toVector: Seq[Double] = Seq(-2, 0)
}
object SkipRight extends Move { override def apply(x: Int, y: Int): (Int, Int) = (x+2, y)
  override val id: Int = 8
  override val toVector: Seq[Double] = Seq(2, 0)
}
