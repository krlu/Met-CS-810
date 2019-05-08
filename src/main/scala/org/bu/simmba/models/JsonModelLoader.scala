package org.bu.simmba.models

import play.api.libs.json._

import scala.io.Source

trait JsonModelLoader{

  val paramsFile: String
  val useGenerativeParams: Boolean

  lazy val paramsMap: Map[String, List[Double]] =
    if(useGenerativeParams) loadGenerativeParams(paramsFile)
    else loadBayesianParams(paramsFile)


  private def loadBayesianParams(paramsFile: String): Map[String, List[Double]] = {
    val source: String = Source.fromFile(paramsFile).getLines.mkString
    val json = Json.parse(source).as[JsObject]
    json.value("allParameters").as[JsArray].value.map { x: JsValue =>
      val dirichletObj = x.as[JsObject].value("Dirichlet").as[JsObject]
      val dirichletMap = dirichletObj.value
      dirichletMap("name").as[JsString].value -> dirichletMap("alphaValues").as[JsArray].value.toList
        .map(_.toString().toDouble)
    }.toMap
  }

  private def loadGenerativeParams(paramsFile: String): Map[String, List[Double]] = {
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

