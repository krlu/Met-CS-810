package org.bu.met810.types.boardassets

import org.bu.met810.types.Vectorizable

case class Building(position: (Int, Int), length: Int, width: Int, override val id: Int = 0) extends Vectorizable {
  val toVector: Seq[Double] = Seq(position._1, position._2, length, width).map(_.toDouble)
}
