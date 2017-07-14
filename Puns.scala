package com.thoughtbot

import play.api.libs.json._
import scala.io.Source
import scala.util.{Try}

case class Rhyme(word: String, score: Int)
case class Pun(pun: String, sourcePhrase: String)

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

    val scores = allRhymes.map { _.score }
    val maxScore = Try(scores.max).toOption

    maxScore match {
      case Some(score) => allRhymes.filter { _.score == score }
      case None => List()
    }
  }

  def loadPhrases(): List[String] = {
    List(
      "beatles-songs.txt",
      "best-selling-books.txt",
      "movie-quotes.txt",
      "oscar-winning-movies.txt",
      "wikipedia-idioms.txt"
    ).flatMap {
      Source.fromFile(_).getLines.toList
    }
  }

  def getPuns(seedWord: String): List[Pun] = {
    val bestRhymes = getBestRhymes(seedWord)
    val rhymeWords = bestRhymes.map { _.word }

    val linesWithOptionMatch = loadPhrases.map(line => {
      val commonWords = line.split(" ").map(_.toLowerCase).intersect(rhymeWords)
      (line, commonWords.headOption)
    }).filter(line => {
      line match {
        case (_, None) => false
        case _ => true
      }
    })

    linesWithOptionMatch.map(lineWithMatch => {
      lineWithMatch match {
        case (line, Some(matchWord)) => {
          val pun = line.
            replaceAllLiterally(matchWord.capitalize, seedWord.capitalize).
            replaceAllLiterally(matchWord, seedWord.toLowerCase)
          Pun(pun, line)
        }
        case (line, _) => Pun("failure", "This SHOULD NOT HAPPEN")
      }
    })
  }

  def main(args: Array[String]) {
    val seedWord = args.head
    val puns = getPuns(seedWord)
    puns.foreach(pun => {
      val punPhrase = pun.pun
      val sourcePhrase = pun.sourcePhrase
      println(f"$punPhrase (pun of '$sourcePhrase')")
    })
  }
}
