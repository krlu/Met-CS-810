package org.bu.met810.models.mcts

case class Node[T](private var children: Map[T, Node[T]] = Map.empty[T, Node[T]],
                   private var wins: Int = 0, private var losses: Int = 0){
  def value: Double = if(wins + losses == 0) 0 else wins.toDouble/(wins + losses).toDouble

  def addWin(): Unit = wins += 1
  def addLoss(): Unit = losses += 1
  def addChild(action: T, node: Node[T]): Unit = children = children ++ Map(action -> node)

  def getChildren: Map[T, Node[T]] = children
  def getWins: Int = wins
  def getLosses: Int = losses
}