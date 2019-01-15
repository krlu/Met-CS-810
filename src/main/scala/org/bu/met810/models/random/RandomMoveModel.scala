package org.bu.met810.models.random

import org.bu.met810.choose
import org.bu.met810.models.PlayerModel

trait RandomMoveModel[Env, Agent, Action] extends PlayerModel[Env, Agent, Action]{
  def moveSet(a: Agent, env: Env): List[Action]
  def getPlayer(playerId: Int, env: Env): Option[Agent]
  def selectMove(playerId: Int, env: Env): Action = {
    val player = getPlayer(playerId, env) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }
    choose(moveSet(player, env))
  }
}