package org.bu.met810.models

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Select, Universe}
import com.cra.figaro.library.atomic.continuous.{AtomicDirichlet, Dirichlet}
import com.cra.figaro.patterns.learning.ModelParameters
import org.bu.met810.types.boardassets.{Board, Player}
import org.bu.met810.types.moves.Move
import play.api.libs.json._

import scala.io.Source

class BayesianPlayerModel(paramsFile: String, val useGenerativeParams: Boolean) extends PlayerModel[Board, Player, Move]{


  val paramsMap: Map[String, List[Double]] = 
    if(useGenerativeParams) loadLearnedParams(paramsFile)
    else{
      val source: String = Source.fromFile(paramsFile).getLines.mkString
      val json = Json.parse(source).as[JsObject]
      json.value("allParameters").as[JsArray].value.map { x: JsValue =>
        val dirichletObj = x.as[JsObject].value("Dirichlet").as[JsObject]
        val dirichletMap = dirichletObj.value
        dirichletMap("name").as[JsString].value -> dirichletMap("alphaValues").as[JsArray].value.toList
          .map(_.toString().toDouble)
      }.toMap
    }

  def loadLearnedParams(paramsFile: String): Map[String, List[Double]] = {
    val bufferedSource = scala.io.Source.fromFile(paramsFile)
    val paramsMap = parseParamsString(bufferedSource.getLines().mkString)
    bufferedSource.close
    paramsMap
  }

  private def parseParamsString(paramsString: String): Map[String, List[Double]] = {
    val json = Json.parse(paramsString).asInstanceOf[JsObject]
    json.fields.map{ case (k, jsValue) =>
      k -> jsValue.asInstanceOf[JsArray].value.map(_.toString().toDouble).toList
    }.toMap
  }
  

  val modelParams = ModelParameters()
  paramsMap.map{case(k,v) => Dirichlet(v:_*)(k, modelParams)}

  override def selectMove(playerId: Int, board: Board): Move = {
    Universe.createNew()

    val player: Player = Set(board.p1, board.p2).find(_.id == playerId) match {
      case Some(p) => p
      case None =>  throw new NoSuchElementException(s"unable to find player with id $playerId!")
    }

    val validMoves: List[Move] = player.moves.filter{ m: Move =>
      val (x1, y1) = m(player.position._1, player.position._2)
      x1 >= 0 && x1 < board.width && y1 >= 0 && y1 < board.length
    }

    val (x1, y1) = board.p1.position
    val (x2, y2) = board.p2.position
    val params = modelParams.getElementByReference(s"${playerId}_${List(x1,y1,x2,y2).mkString("_")}_move").asInstanceOf[AtomicDirichlet]
    val moveDist = Select(params, player.moves:_*)
    val alg = Importance(300, moveDist)
    alg.start()
    val computedDist = alg.distribution(moveDist).toList
    val desiredMove = computedDist.filter{case (_ ,m) => validMoves.contains(m)}.sortWith(_._1 > _._1).head._2
    alg.stop()
    alg.kill()
    desiredMove
  }
}
