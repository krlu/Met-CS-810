package org.bu.met810.models.mcts

case class Node[T](private var children: Map[T, Node[T]] = Map.empty[T, Node[T]],
                   private var wins: Int = 0, private var numVisits: Int = 0){
  def value(totalVisits: Int): Double =
    if(numVisits == 0) Int.MaxValue
    else wins.toDouble/ numVisits.toDouble + Math.sqrt(2) * Math.sqrt(Math.log(totalVisits)/numVisits.toDouble)

  def addWin(): Unit = wins += 1
  def addVisit(): Unit = numVisits += 1
  def addChild(action: T, node: Node[T]): Unit = children = children ++ Map(action -> node)

  def getChildren: Map[T, Node[T]] = children
  def getWins: Int = wins
  def getNumVisits: Int = numVisits
}