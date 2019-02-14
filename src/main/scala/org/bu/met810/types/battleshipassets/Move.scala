package org.bu.met810.types.battleshipassets

import org.bu.met810.types.Action

sealed case class Move(pos: (Int, Int)) extends Action