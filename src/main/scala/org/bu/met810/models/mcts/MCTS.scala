package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment, Vectorizable}
import org.bu.met810.choose

class MCTS[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action]
(sim: Simulator[Env, A, Action], possibleMoves: List[Action], numPlayouts: Int = 300) extends PlayerModel[Env, A , Action]{
  var root: Node[Action] = _
  var path = List.empty[Node[Action]]
  var numIterations = 0

  def playout(currState: Env, player: Agent): Int = {
    sim.setBoard(currState)
    sim.runFullGame() match {
      case Some(winner) => if(winner.id == player.id) 1 else -1
      case _ => 0
    }
  }

  def expandTree(player: Agent, currState: Env): Option[Int] = {
    sim.setBoard(currState)
    var currentNode = root
    while(sim.getTurn != player.id && sim.getWinner.isEmpty) sim.runStep()
    val newStateOpt: Option[(Env, Action, Env)] = sim.runStep()
    newStateOpt match {
      case Some((_, move, newState)) =>
        currentNode = currentNode.getChildren(move)
        val result = playout(newState, player)
        if(result > 0) currentNode.addWin() else currentNode.addLoss()
        Some(result)
      case None =>
        if(sim.getWinner.get.id == player.id) {
          currentNode.addWin()
          Some(1)
        }
        else {
          currentNode.addLoss()
          Some(-1)
        }
    }
  }


  override def selectMove(agent: A, e: Env): Action = {
    val children = possibleMoves.map{ _ -> Node[Action]()}.toMap
    root = Node[Action](children)
    for(i <- 0 until numPlayouts){
      numIterations = i
      expandTree(agent, e)
    }
//    println(root.getChildren)
    if(root.getChildren.exists(_._2.getWins > 0)) {
      val (maxMove, _) = root.getChildren.maxBy(_._2.value)
      maxMove
    }
    else choose(possibleMoves.filter(e.isValidAction(_, agent)))
  }
}