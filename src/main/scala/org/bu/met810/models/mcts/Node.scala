package org.bu.met810.models.mcts

case class Node[T, U](state: T,
                      private var children: Map[U, Node[T, U]] = Map.empty[U, Node[T, U]],
                      private var wins: Int = 0, private var losses: Int = 0){
  def value: Double = wins.toDouble/(wins + losses).toDouble
  def addWin(): Unit = wins += 1
  def addLoss(): Unit = losses += 1
  def addChild(action: U, node: Node[T, U]): Unit = {
    children = children ++ Map(action -> node)
  }
  def getChildren: Map[U, Node[T, U]] = children
  def getWins: Int = wins
  def getLosses: Int = losses
}

object Node{
  def apply[T, U](state: T, children: Map[U, Node[T, U]] = Map.empty[U, Node[T, U]], wins: Int = 0,losses: Int = 0): Node[T, U] =
    new Node[T, U](state, children, wins, losses)
}