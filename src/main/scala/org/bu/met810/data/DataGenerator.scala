package org.bu.met810.data

import java.io.PrintWriter

import org.bu.met810.model.RandomMoveModel
import org.bu.met810.types.boardassets._
import org.bu.met810.types.moves.Move

object DataGenerator {
  def main(args: Array[String]): Unit = {
    val outputFilePath: String = "results.csv"
    val board = Board(Robber((0, 0)), Cop((9, 9)), 10, 10, Seq.empty[Building])
    val sim = Simulator(board, RandomMoveModel(), RandomMoveModel())
    var result: Option[(Board, Move)] = None
    val pw = new PrintWriter(outputFilePath)
    do{
      result = sim.runSimulator()
      result match{
        case None =>
        case Some((b, m)) =>
          val (boardVector, moveVector) = (b.toVector, m.toVector)
          pw.append(s"${boardVector.mkString(",")},${moveVector.mkString(",")}\n")
      }
    } while(result.nonEmpty)
    pw.close()
  }
}
