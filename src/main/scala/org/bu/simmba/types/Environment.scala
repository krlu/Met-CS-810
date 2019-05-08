package org.bu.simmba.types

trait Environment[Action, A <: Agent] {
  val p1: A
  val p2: A
  def isValidAction(action: Action, agent: A): Boolean
}
