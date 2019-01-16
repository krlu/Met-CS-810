package org.bu.met810.types.battleshipassets

sealed case class Board(p1: Player, p2: Player, width: Int, height: Int)