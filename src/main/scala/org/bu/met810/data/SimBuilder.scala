package org.bu.met810.data

import org.bu.met810.models.PlayerModel


trait SimBuilder[Env, Agent, Action]{

  def randomInitialization(p1Model: PlayerModel[Env, Agent, Action],
                           p2Model: PlayerModel[Env, Agent, Action],
                           envSize: Int, shouldApplyNoise: Boolean): Simulator[Env, Agent, Action]

  def runBatch(p1Model: PlayerModel[Env, Agent, Action],
               p2Model: PlayerModel[Env, Agent, Action],
               numTrials: Int = 1000, envSize: Int = 4,
               shouldApplyNoise: Boolean): Seq[Agent] = {
    val start = System.currentTimeMillis()
    val winners: Seq[Agent] = {
      for(_ <- 1 to numTrials) yield {
        val sim = randomInitialization(p1Model, p2Model, envSize, shouldApplyNoise)
        sim.runFullGame()
      }
    }.flatten
    val end = System.currentTimeMillis()
    println(s"runtime: ${(end - start).toDouble/1000}")
    winners
  }
}
