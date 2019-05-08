package org.bu.simmba.models

import play.api.libs.json.{JsArray, JsNumber, JsObject}

/**
  * Data structure that represents a complete Bipartite graph from a set of domain vertices to codomain vertices
  * @param edges - edges in bipartite graph, maps from domain to codomain
  * @param domain - set of states to map from
  * @param codomain - set of states to map to
  * @tparam T - domain data type
  * @tparam U - codomain data type
  */
class BipartiteModel[T, U](val edges: List[((T, U), Double)], val domain: Seq[T], val codomain: Seq[U]){
  def asJson(keySuffix: String = ""): JsObject = {
    val x = domain.map{ v =>
      val pairs = mapForm.getOrElse(v, List())
      val keyName: String = v match {
        case tuple: (Any, Any) =>
          val (a, b) = tuple
          s"${a}_$b"
        case tuple: (Any, Any, Any) =>
          val (a, b, c) = tuple
          s"${a}_${b}_$c"
        case tuple: (Any, Any, Any, Any) =>
          val (a, b, c, d) = tuple
          s"${a}_${b}_${c}_$d"
        case tuple: (Any, Any, Any, Any, Any) =>
          val (a, b, c, d, e) = tuple
          s"${a}_${b}_${c}_${d}_$e"
        case list: List[Any] => list.mkString("_")
        case _ => v.toString
      }
      keyName + keySuffix -> JsArray(codomain.map(v1 => JsNumber(pairs.find(_._1 == v1).get._2)))
    }
    JsObject(x)
  }

  lazy val mapForm: Map[T, Seq[(U, Double)]] = domain.map{ n =>
    val trans: Seq[(U, Double)] = edges.filter(_._1._1 == n).map{case ((_, n2), wt) =>
      (n2, wt)
    }
    n -> trans
  }.toMap
}

/**
  * Training process simply observes each pair of values, one from each of domain and codomain
  * Edge weights all initialized to 1.0 and incremented by 1.0 for each pair observed
  */
object BipartiteModel{
  def apply[T, U](data: Seq[Seq[(T, U)]], domain: Seq[T], codomain: Seq[U]): BipartiteModel[T, U] = {
    var graph: Map[(T, U), Double] = domain.flatMap { t =>
      codomain.map{ u =>
        ((t, u), 1.0)
      }
    }.toMap
    for{
      row <- data
      (t, u) <- row
    } {
      val list: Seq[((T, U), Double)] = graph.toSeq ++ Map((t,u) -> 1.0).toSeq
      graph = list.groupBy { case (k, _) => k }.mapValues(_.map { case (_, v) => v }.sum)
    }
    new BipartiteModel[T, U](graph.toList, domain, codomain)
  }
}
