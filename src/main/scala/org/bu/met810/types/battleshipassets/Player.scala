package org.bu.met810.types.battleshipassets

import org.bu.met810.types.{Agent, Vectorizable}

sealed case class Player(positions: List[(Int, Int)], id: Int,
                         movesMade: List[Move] = List.empty[Move]) extends Agent with Vectorizable {
  def hasDestroyedOpponent(player: Player): Boolean = player.positions.forall(p => movesMade.map(_.pos).contains(p))

  override val toVector: Seq[Double] =
    positions.flatMap(p => List(p._1.toDouble, p._2.toDouble)) ++ movesMade.flatMap(_.toVector)
}
