package org.bu.met810.model.clasical

import java.io.FileWriter

import com.cra.graymatter.models.BipartiteModel
import org.bu.met810.types.moves.Move
import play.api.libs.json._

object GenerativeModelLearner {

  def main(args: Array[String]): Unit = {

    val dataFile = args(0)
    val paramsFile = args(1)
    val bufferedSource = scala.io.Source.fromFile(dataFile)

    println("training player model...")
    val playerModel: BipartiteModel[(Int, Int), Move] = ???

    val combinedJson: JsValue = JsObject(playerModel.asJson("_workflow").value)
    printJsonString(combinedJson, paramsFile, append = false)
  }

  def loadLearnedParams(paramsFile: String): Map[String, Seq[Double]] = {
    val bufferedSource = scala.io.Source.fromFile(paramsFile)
    val paramsMap = parseParamsString(bufferedSource.getLines().mkString)
    bufferedSource.close
    paramsMap
  }

  private def parseParamsString(paramsString: String): Map[String, Seq[Double]] = {
    val json = Json.parse(paramsString).asInstanceOf[JsObject]
    json.fields.map{ case (k, jsValue) =>
      k -> jsValue.asInstanceOf[JsArray].value.map(_.toString().toDouble)
    }.toMap
  }

  private def printJsonString(json: JsValue, outfile: String, append: Boolean = true): Unit ={
    val pw = new FileWriter(outfile, append)
    pw.append(s"$json\n")
    pw.close()
  }
}