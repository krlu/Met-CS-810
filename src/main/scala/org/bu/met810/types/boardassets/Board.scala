package org.bu.met810.types.boardassets

case class Board(p1: Player, p2: Player, length: Int, width: Int, objects: Seq[Building])
