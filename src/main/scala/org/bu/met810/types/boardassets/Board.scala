package org.bu.met810.types.boardassets

import org.bu.met810.types.Vectorizable

case class Board(p1: Player, p2: Player, length: Int, width: Int,
                 objects: Seq[Building], override val id: Int = 0) extends Vectorizable {
  val toVector: Seq[Double] = vectorizeAgents(p1, p2) ++ Seq(length, width).map(_.toDouble) ++ objects.flatMap(_.toVector)
  private def vectorizeAgents(players: Player*): Seq[Double] = players.flatMap(_.toVector)
}
