package com.thoughtbot

import play.api.libs.json._
import scala.io.Source

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

  def getBestRhymes(word: String): List[Rhyme] = {
    val allRhymes = getRhymes(word)
    val maxScore = allRhymes.map { _.score }.max
    allRhymes.filter { _.score == maxScore }
  }

  def main(args: Array[String]) {
    val bestRhymes = getBestRhymes("beer")
    val rhymeWords = bestRhymes.map { _.word }
    val lines = Source.fromFile("beatles_songs.txt").getLines.toList

    val linesWithOptionMatch = lines.map(line => {
      val commonWords = line.split(" ").map(_.toLowerCase).intersect(rhymeWords)
      (line, commonWords.headOption)
    }).filter(line => {
      line match {
        case (line, None) => false
        case _ => true
      }
    })

    // println(
    //   matchingLines.map(line => {
    //     val matchWord = 
    //   })
    // )
  }
}
