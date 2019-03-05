package org.bu.met810.types

trait Agent{
  val id: Int
  val positions: List[(Int, Int)]
  val state: Seq[Int]
}
