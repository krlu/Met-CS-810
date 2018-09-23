package org.bu.met810.model

trait PlayerModel[Env, Agent, Action]{
  def selectMove(assetId: Int, e: Env): Action
}
