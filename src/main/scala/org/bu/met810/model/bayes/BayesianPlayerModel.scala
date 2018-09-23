package org.bu.met810.model.bayes

import org.bu.met810.model.PlayerModel
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move

class BayesianPlayerModel extends PlayerModel[Board, Player, Move]{
  override def selectMove(assetId: Int, e: Board): Move = ???
}
