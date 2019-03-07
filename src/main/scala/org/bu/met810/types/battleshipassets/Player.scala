package org.bu.met810.types.battleshipassets

import org.bu.met810.types.{Agent, Vectorizable}

sealed case class Player(positions: List[(Int, Int)], id: Int,
                         movesMade: Map[(Int, Int), Int],
                         positionsDestroyed: List[(Int, Int)] = List.empty[(Int, Int)]) extends Agent with Vectorizable {
  def isDestroyed: Boolean = positionsDestroyed.distinct == positions.distinct
  override val state: Seq[Int] = positions.flatMap{ p =>
    val isAlive = if(positionsDestroyed.contains(p)) 0 else 1
    List(p._1, p._2, isAlive) ++ movesMade.toList.flatMap{case ((x,y), moveMade) => List(x,y,moveMade)}
  }
  override val toVector: Seq[Double] = state.map(_.toDouble)
}

