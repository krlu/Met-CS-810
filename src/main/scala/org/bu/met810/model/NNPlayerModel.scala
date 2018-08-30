package org.bu.met810.model

trait NNPlayerModel[Env, Asset, Action] extends PlayerModel[Env, Action]{
  def vectorizeEnvironment(e: Env): Seq[Double]
  def vectorizeAsset(a: Asset*): Seq[Double]
  def vectorToMove(vector: Seq[Double]): Action
}
