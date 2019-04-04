package org.bu.met810

import org.bu.met810.data.CopsAndRobbersSim
import org.bu.met810.models.mcts.{MCTS, Node}
import org.bu.met810.models.random.RandomMoveModel
import org.bu.met810.types.copsandrobbersassets._
import org.scalatest.{FlatSpec, Matchers}

class MCTSTest extends FlatSpec with Matchers{
  "Tree structure" should "update values" in {
    var parentOf = Map.empty[Node[Int], Node[Int]]
    def update(leaf: Node[Int], isWin: Boolean): Unit = {
      var currentNode = leaf
      while(parentOf.contains(currentNode)){
        currentNode = parentOf(currentNode)
        if(isWin) currentNode.addWin()
        else currentNode.addLoss()
      }
    }
    val root = Node[Int]()
    val child1 = Node[Int]()
    val child2 = Node[Int]()

    root.addChild(0,child1)
    child1.addChild(0, child2)
    parentOf = parentOf ++ Map(child1 -> root)
    parentOf = parentOf ++ Map(child2 -> child1)

    assert(root.getChildren.size == 1)
    assert(root.getChildren(0) == child1)
    assert(parentOf(child1) == root)
    assert(parentOf.size == 2)

    update(child1, isWin = true)
    assert(root.getWins == 1)
    assert(root.getNumVisits == 0)
    assert(child1.getWins == 0)
    assert(child1.getNumVisits == 0)

    update(child2, isWin = false)
    assert(child1.getWins == 0)
    assert(child1.getNumVisits == 1)
    assert(root.getWins == 1)
    assert(root.getNumVisits == 1)
  }
  "MCTS" should "find optimal moves" in {
    val sim = new CopsAndRobbersSim(
      initialBoard = Board(Robber((0,0)), Cop((1,1)), 1,1, Seq.empty[Building]),
      RandomMoveModel.crModel(Move.robberMoves),
      RandomMoveModel.crModel(Move.copMoves))
    val robberModel = new MCTS(sim, Move.robberMoves)
    val winners: Seq[Player] = CopsAndRobbersSim.runBatch(robberModel, RandomMoveModel.crModel(Move.copMoves), numTrials = 1000)
    val robberWins = winners.count(_.id == 0)
    val copWins =  winners.count(_.id == 1)
    println(robberWins, copWins)
  }
}
