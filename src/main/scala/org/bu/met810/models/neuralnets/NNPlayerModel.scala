package org.bu.met810.models.neuralnets

import org.bu.met810.models.PlayerModel

trait NNPlayerModel[Env, Agent, Action] extends PlayerModel[Env, Agent, Action]{
  def vectorToMove(vector: Seq[Double]): Action
}
