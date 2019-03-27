package org.bu.met810.models.mcts

import org.bu.met810.data.Simulator
import org.bu.met810.models.PlayerModel
import org.bu.met810.types.{Agent, Environment, Vectorizable}
import org.bu.met810.choose

class MCTS[Env <: Environment[Action, A] with Vectorizable, A <: Agent, Action]
(sim: Simulator[Env, A, Action], possibleMoves: Set[Action], numPlayouts: Int = 100) extends PlayerModel[Env, A , Action]{
  var root: Node[Env, Action] = _
  var parentOf = Map.empty[Node[Env, Action], Node[Env, Action]]

  def playout(currState: Env, player: Agent): Int = {
    sim.setBoard(currState)
    sim.runFullGame() match {
      case Some(winner) => if(winner.id == player.id) 1 else -1
      case _ => 0
    }
  }

  def expandTree(player: Agent): Option[(Node[Env, Action], Int)] = {
    var currentNode = root
    while(currentNode.getChildren.exists(c => c._2.value > 0.2)) {
      currentNode = currentNode.getChildren.maxBy(_._2.value)._2
    }
    sim.setBoard(currentNode.state)

    while (sim.getTurn != player.id && sim.getWinner.isEmpty) sim.runStep()

    if(sim.getWinner.nonEmpty){
      val result = if(sim.getWinner.get == player) {
        currentNode.addWin()
        1
      }
      else {
        currentNode.addLoss()
        -1
      }
      Some((currentNode, result))
    }
    else {
      val x = sim.runStep()
      x match {
        case Some((_, action, newState)) =>
          val result = playout(newState, player)
          val leaf = Node[Env, Action](newState)
          if (result > 0) leaf.addWin()
          if (result < 0) leaf.addLoss()
          if(!currentNode.getChildren.values.toList.contains(leaf)){
            currentNode.addChild(action, leaf)
            parentOf = parentOf ++ Map(leaf -> currentNode)
          }
          Some((leaf, result))
        case None => None
      }
    }
  }

  def update(leaf: Node[Env, Action], isWin: Boolean): Unit = {
    var currentNode = leaf
    while(parentOf.contains(currentNode)){
      currentNode = parentOf(currentNode)
      if(isWin) currentNode.addWin()
      else currentNode.addLoss()
    }
  }

  override def selectMove(agent: A, e: Env): Action = {
    parentOf = Map.empty[Node[Env, Action], Node[Env, Action]]
    root = Node[Env, Action](e)
    for(_ <- 0 until numPlayouts){
      expandTree(agent) match {
        case Some((leaf, result)) => update(leaf, result > 0)
        case None =>
      }
    }
//    println(root.getChildren.toList.map(_._2.value))
    if(root.getChildren.nonEmpty) root.getChildren.maxBy(_._2.value)._1
    else choose(possibleMoves.toList.filter(e.isValidAction(_, agent)))
  }
}