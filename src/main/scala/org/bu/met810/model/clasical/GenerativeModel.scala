package com.cra.graymatter.models

/**
  * Predicts discrete events via un-supervised learning by building a complete graph over all possible events
  * Extends BipartiteModel but assumes the domain and codomain are the same
  */
class GenerativeModel[T](edges: List[((T, T), Double)], domain: Seq[T]) extends BipartiteModel[T, T](edges, domain, domain)

/**
  * Unlike a bipartite example, the current training process does not rely only on ordered pair observations
  * Instead, the training process relies on temporal relationships between states
  */
object GenerativeModel{
  def apply[T](data: Seq[Seq[T]], possibleNodes: Seq[T]): GenerativeModel[T] = {
    var graph: Map[(T, T), Double] = possibleNodes.flatMap { t1 =>
      possibleNodes.map { t2 => (t1, t2) -> 1.0 }
    }.toMap
    for (i <- data.indices) {
      val row = data(i)
      for(j <- 0 until row.size - 1) {
        val list: Seq[((T, T), Double)] = graph.toSeq ++ Map((row(j), row(j + 1)) -> 1.0).toSeq
        graph = list.groupBy { case (k, _) => k }
          .mapValues(_.map { case (_, v) => v }.sum)
      }
    }
    new GenerativeModel[T](graph.toList, possibleNodes)
  }
}