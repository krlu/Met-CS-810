package org.bu.met810.model.neuralnets

import org.bu.met810.model.PlayerModel

trait NNPlayerModel[Env, Agent, Action] extends PlayerModel[Env, Agent, Action]{
  def vectorToMove(vector: Seq[Double]): Action
}
