package org.bu.met810.types

trait Environment[A <: Action, Agent] {
  def isValidAction(action: A, agent: Agent): Boolean
}
