package org.bu.met810.models.mcts

case class Node[T](value: Double, move: T, children: List[Node[T]])
