package org.bu.met810.types.battleshipassets

import org.bu.met810.types.Vectorizable

sealed case class Move(pos: (Int, Int)) extends Vectorizable {
  override val toVector: Seq[Double] = Seq(pos._1, pos._2)
}