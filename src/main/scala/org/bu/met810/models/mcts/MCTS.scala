package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment, Vectorizable}
import org.bu.met810.choose

class MCTS[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action]
(sim: Simulator[Env, A, Action], possibleMoves: List[Action], numPlayouts: Int = 300) extends PlayerModel[Env, A , Action]{
  var root: Node[Action] = _
  var path = List.empty[Node[Action]]
  var playoutsSoFar = 0

  def playout(currState: Env, player: Agent): Boolean = {
    sim.runFullGame() match {
      case Some(winner) => winner.id == player.id
      case _ => throw new IllegalStateException("")
    }
  }

  def expandTree(player: Agent, currState: Env): Unit = {
    sim.setBoard(currState,player.id)
    var currentNode = root
//    while(sim.getTurn != player.id && sim.getWinner.isEmpty) sim.runStep()
    val newStateOpt: Option[(Env, Action, Env)] = sim.runStep()
    newStateOpt match {
      case Some((_, move, newState)) =>
        currentNode = currentNode.getChildren(move)
        currentNode.addVisit()
        val result = playout(newState, player)
        if(result) currentNode.addWin()
      case None =>
    }
  }

  override def selectMove(agent: A, e: Env): Action  = {
    val children = possibleMoves.map{ _ -> Node[Action]()}.toMap
    root = Node[Action](children)
    for(i <- 0 until numPlayouts){
      expandTree(agent, e)
      playoutsSoFar = i
    }
    if(root.getChildren.exists(x => e.isValidAction(x._1, agent))) {
      val (maxMove, _) = root.getChildren.filter(x => e.isValidAction(x._1, agent)).maxBy(_._2.value(playoutsSoFar))
      maxMove
    }
    else choose(possibleMoves.filter(e.isValidAction(_, agent)))
  }

  override def modelName = "MCTS"
}

object MCTS{
  def apply[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action]
  (sim: Simulator[Env, A, Action], possibleMoves: List[Action], numPlayouts: Int = 300): MCTS[Env, A, Action] =
    new MCTS(sim, possibleMoves, numPlayouts)
}