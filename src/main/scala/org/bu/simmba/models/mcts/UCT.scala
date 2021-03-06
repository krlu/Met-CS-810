package org.bu.simmba.models.mcts

object UCT {
  def findBestNodeWithUCT[T](node: Node[T]): (T, Node[T]) = {
    val parentVisit = node.getNumVisits
    node.getChildren.maxBy(_._2.value(parentVisit))
  }
}