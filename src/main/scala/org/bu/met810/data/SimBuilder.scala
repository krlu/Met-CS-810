package org.bu.met810.data

import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment, Vectorizable}


trait SimBuilder[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action]{

  def randomInitialization(p1Model: PlayerModel[Env, A, Action],
                           p2Model: PlayerModel[Env, A, Action],
                           envSize: Int, shouldApplyNoise: Boolean): Simulator[Env, A, Action]

  def runBatch(p1Model: PlayerModel[Env, A, Action],
               p2Model: PlayerModel[Env, A, Action],
               numTrials: Int = 1000, envSize: Int = 4,
               shouldApplyNoise: Boolean = false): Seq[A] = {
    val start = System.currentTimeMillis()
    val winners: Seq[A] = {
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
