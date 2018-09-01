package org.bu.met810.model

trait NNPlayerModel[Env, Agent, Action] extends PlayerModel[Env, Action]{
  def vectorToMove(vector: Seq[Double]): Action
}
