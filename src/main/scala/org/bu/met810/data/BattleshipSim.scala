package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.copsandrobbersassets.{Board, Move, Player}

class BattleshipSim extends Simulator[Board, Player, Move]{
  override var board: Board = _
  override val model1: PlayerModel[Board, Player, Move] = ???
  override val model2: PlayerModel[Board, Player, Move] = ???
  override var turn: Int = 0
  override val shouldApplyNoise: Boolean = false

  /**
    * Performs one turn update in the simulation
    *
    * @return
    */
  override def runSimulator(): Option[(Board, Move, Board)] = ???
}
