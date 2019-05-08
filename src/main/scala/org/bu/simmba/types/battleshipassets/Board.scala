package org.bu.simmba.types.battleshipassets

import org.bu.simmba.types.{Environment, Vectorizable}

sealed case class Board(p1: Player, p2: Player,
                        width: Int, height: Int) extends Environment[Move, Player] with Vectorizable{
  override def isValidAction(action: Move, agent: Player): Boolean = {
    val (x,y) = action.pos
    !agent.movesMade.filter(_._2 >= 0).map(_._1).contains(action.pos) && x >= 0 && x < width && y >= 0 && y < width
  }
  override val toVector: Seq[Double] = vectorizeAgents(p1, p2) ++ Seq(height, width).map(_.toDouble)
  private def vectorizeAgents(players: Player*): Seq[Double] = players.flatMap(_.toVector)

}