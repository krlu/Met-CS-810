package org.bu.met810.models

import play.api.libs.json._

import scala.io.Source

/**
  * Predicts discrete events via un-supervised learning by building a complete graph over all possible events
  * Extends BipartiteModel but assumes the domain and codomain are the same
  */
trait JsonModelLoader{

  val paramsFile: String
  val useGenerativeParams: Boolean

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

  private def loadLearnedParams(paramsFile: String): Map[String, List[Double]] = {
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

}

