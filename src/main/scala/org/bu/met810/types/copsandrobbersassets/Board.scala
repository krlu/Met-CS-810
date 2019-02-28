package org.bu.met810.types.copsandrobbersassets

import org.bu.met810.types.{Environment, Vectorizable}

case class Board(p1: Player, p2: Player, length: Int, width: Int,
                 objects: Seq[Building], override val id: Int = 0) extends Vectorizable with Environment[Move, Player] {
  val toVector: Seq[Double] = vectorizeAgents(p1, p2) ++ Seq(length, width).map(_.toDouble) ++ objects.flatMap(_.toVector)

  private def vectorizeAgents(players: Player*): Seq[Double] = players.flatMap(_.toVector)

  override def isValidAction(move: Move, player: Player): Boolean = {
    val pos = move(player.positions.head._1, player.positions.head._2)
    val (x1, y1) = pos
    x1 >= 0 && x1 < width && y1 >= 0 && y1 < length
  }
}
