package org.bu.simmba

import org.bu.simmba.simulation.CopsAndRobbersSim
import org.bu.simmba.models.mcts.{MCTS, Node}
import org.bu.simmba.models.random.RandomMoveModel
import org.bu.simmba.types.copsandrobbersassets._
import org.scalatest.{FlatSpec, Matchers}

class MCTSTest extends FlatSpec with Matchers{
  "Tree structure" should "update values" in {
    var parentOf = Map.empty[Node[Int], Node[Int]]
    def update(leaf: Node[Int], isWin: Boolean): Unit = {
      var currentNode = leaf
      while(parentOf.contains(currentNode)){
        currentNode = parentOf(currentNode)
        if(isWin) currentNode.addWin()
        else currentNode.addVisit()
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
    val sim1 = CopsAndRobbersSim.randomInitialization(
      RandomMoveModel.crModel(Move.robberMoves),
      RandomMoveModel.crModel(Move.copMoves))
    val sim2 = CopsAndRobbersSim.randomInitialization(
      RandomMoveModel.crModel(Move.robberMoves),
      RandomMoveModel.crModel(Move.copMoves))
    val results = for {
      mctsModel1 <- List(RandomMoveModel.crModel(Move.robberMoves), MCTS(sim1, Move.robberMoves))
      mctsModel2 <- List(RandomMoveModel.crModel(Move.robberMoves), MCTS(sim2, Move.copMoves, numPlayouts = 100))
    } yield {
      val winners: Seq[Player] = CopsAndRobbersSim.runBatch(mctsModel1, mctsModel2, numTrials = 10000)
      val robberWins = winners.count(_.id == 0)
      val copWins = winners.count(_.id == 1)
      (robberWins, copWins)
    }
    val (rw0, cw0) = results.head
    val (rw1, cw1) = results(1)
    val (rw2, cw2) = results(2)
    val (rw3, cw3) = results(3)
    assert(condition = 2500 < rw0 && rw0 < 3000 && rw0 + cw0 == 10000)
    assert(condition = 1000 < rw1 && rw1 < 2000 && rw1 + cw1 == 10000)
    assert(condition = 8500 < rw2 && rw2 < 9500 && rw2 + cw2 == 10000)
    assert(condition = 5000 < rw3 && rw3 < 6000 && rw3 + cw3 == 10000)
  }
}
