package org.bu.met810.models

import org.bu.met810.types.Action

trait PlayerModel[Env, Agent, A <: Action]{
  def selectMove(agent: Agent, e: Env): A
}
