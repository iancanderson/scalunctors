package com.thoughtbot

import play.api.libs.json._

case class Rhyme(word: String, score: Int)

object Puns {
  implicit val rhymeReads = Json.reads[Rhyme]

  def get(url: String) = scala.io.Source.fromURL(url).mkString

  def getJson(url: String): JsValue = {
    val body = get(url)
    Json.parse(body)
  }

  def getRhymes(word: String): List[Rhyme] = {
    val url = f"http://rhymebrain.com/talk?function=getRhymes&word=$word"
    val result = getJson(url)
    val rhymesResult: JsResult[List[Rhyme]] = Json.fromJson[List[Rhyme]](result)

    rhymesResult match {
      case JsSuccess(rhymes: List[Rhyme], path: JsPath) => rhymes
      case e : JsError => List()
    }
  }

  def main(args: Array[String]) {
    val allRhymes = getRhymes("beer")
    val maxScore = allRhymes.map { _.score }.max
    val bestRhymes = allRhymes.filter { _.score == maxScore }
    println(bestRhymes)
  }
}
