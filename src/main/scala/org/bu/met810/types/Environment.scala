package org.bu.met810.types

trait Environment[Action, Agent] {
  def isValidAction(action: Action, agent: Agent): Boolean
}
