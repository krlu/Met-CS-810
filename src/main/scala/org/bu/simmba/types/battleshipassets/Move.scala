package org.bu.simmba.types.battleshipassets

import org.bu.simmba.types.Vectorizable

sealed case class Move(pos: (Int, Int)) extends Vectorizable {
  override val toVector: Seq[Double] = Seq(pos._1, pos._2)
}