package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Action, Environment}


trait SimBuilder[Env <: Environment[A, Agent], Agent, A <: Action]{

  def randomInitialization(p1Model: PlayerModel[Env, Agent, A],
                           p2Model: PlayerModel[Env, Agent, A],
                           envSize: Int, shouldApplyNoise: Boolean): Simulator[Env, Agent, A]

  def runBatch(p1Model: PlayerModel[Env, Agent, A],
               p2Model: PlayerModel[Env, Agent, A],
               numTrials: Int = 1000, envSize: Int = 4,
               shouldApplyNoise: Boolean = false): Seq[Agent] = {
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
