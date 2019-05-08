package org.bu.simmba.models

trait PlayerModel[Env, Agent, Action]{
  def selectMove(agent: Agent, e: Env): Action
  def modelName: String
}
