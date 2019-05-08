package org.bu.simmba.simulation

import org.bu.simmba.models.PlayerModel
import org.bu.simmba.types.{Agent, Environment, Vectorizable}


trait SimBuilder[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action]{

  def randomInitialization(p1Model: PlayerModel[Env, A, Action],
                           p2Model: PlayerModel[Env, A, Action],
                           envSize: Int, shouldApplyNoise: Boolean = false, firstMove: Int = 0): Simulator[Env, A, Action]

  def runBatch(p1Model: PlayerModel[Env, A, Action],
               p2Model: PlayerModel[Env, A, Action],
               numTrials: Int = 1000, envSize: Int = 4,
               shouldApplyNoise: Boolean = false, printTrialNums: Boolean = false): Seq[A] = {
    val winners: Seq[A] = {
      for(i <- 1 to numTrials) yield {
        if(printTrialNums)
          println(i)
        val sim = randomInitialization(p1Model, p2Model, envSize, shouldApplyNoise, firstMove = i%2)
        sim.runFullGame()
      }
    }.flatten
    winners
  }
}
