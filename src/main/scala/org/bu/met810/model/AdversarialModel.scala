package org.bu.met810.model

import org.bu.met810.types.boardassets.Board
import org.bu.met810.types.moves.Move

//TODO: Current uses toy example from Neuroflow site
class AdversarialModel extends PlayerModel{
  override def selectMove(possibleMoves: Set[Move], board: Board): Move = ???

}

