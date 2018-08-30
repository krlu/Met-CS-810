package org.bu.met810.model

trait PlayerModel[Env, Action]{
  def selectMove(assetId: Int, e: Env): Action
}
