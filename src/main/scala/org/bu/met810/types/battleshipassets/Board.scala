package org.bu.met810.types.battleshipassets

import org.bu.met810.types.{Environment, Vectorizable}

sealed case class Board(p1: Player, p2: Player,
                        width: Int, height: Int) extends Environment[Move, Player] with Vectorizable{
  override def isValidAction(action: Move, agent: Player): Boolean = {
    val (x,y) = action.pos
    !agent.movesMade.contains(action) && x >= 0 && x < width && y > 0 && y < width
  }
  override val toVector: Seq[Double] = vectorizeAgents(p1, p2) ++ Seq(height, width).map(_.toDouble)
  private def vectorizeAgents(players: Player*): Seq[Double] = players.flatMap(_.toVector)

}