package org.bu.met810.models

trait PlayerModel[Env, Agent, Action]{
  def selectMove(agent: Agent, e: Env): Action
}
