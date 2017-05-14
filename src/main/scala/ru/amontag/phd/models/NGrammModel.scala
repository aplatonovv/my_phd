package ru.amontag.phd.models

import java.io.PrintWriter

import scala.io.Source

/**
  * Created by amontag on 14.05.2017.
  */
class NGrammModel(val n: Int,
                  val nGrammsStatistics: Map[String, Int],
                  val nMinusOneStatistics: Map[String, Int]) {
  def estimateNGramm(ngramm: String): Double = {
    val nGrammStat = nGrammsStatistics.getOrElse(ngramm, 0).toDouble
    val nMinusOneStat = nMinusOneStatistics.getOrElse(ngramm.take(ngramm.length - 1), 1).toDouble
    nGrammStat / nMinusOneStat
  }

  def estimateSequence(sequence: String): Double = {
    NGrammModel.split(sequence, n).map(estimateNGramm).product
  }

  def estimateJoining(left: String, right: String): Double = {
    assert(left.size + right.size >= n)
    (for (i <- 1 until n) yield {
      left.takeRight(i) + right.take(n - i)
    }).map(estimateNGramm).max
  }

  def serializeTo(filename: String): Unit = NGrammModel.serializeTo(this, filename)

  def split(sequence: String): Seq[String] = NGrammModel.split(sequence, n)
}

object NGrammModel {
  def build(text: String, n: Int, splitter: (String) => Seq[String] = _.split("\n")): NGrammModel = {
    assert(n > 1)
    val processedText = splitter(text).map(_.trim).filter(_.nonEmpty)
    val nGramms: Map[String, Int] = processedText.flatMap(split(_, n)).groupBy(x => x).mapValues(_.size)
    val nMinusOneGramm: Map[String, Int] = processedText.flatMap(split(_, n - 1)).groupBy(x => x).mapValues(_.size)

    new NGrammModel(n, nGramms, nMinusOneGramm)
  }

  private def split(text: String, n: Int): Seq[String] =  (generateStopSequence(n) + text + generateStopSequence(n))
    .sliding(n, 1).toList

  private def generateStopSequence(n: Int): String = (0 until n).map(x => "$").mkString("")

  def serializeTo(nGrammModel: NGrammModel, filename: String): Unit = {
    resource.managed(new PrintWriter(filename)) acquireAndGet { out =>
      out.println(nGrammModel.n)
      out.println(nGrammModel.nGrammsStatistics.size)
      out.println(nGrammModel.nMinusOneStatistics.size)
      nGrammModel.nGrammsStatistics.foreach(t => out.println(s"${t._1} :=: ${t._2}"))
      nGrammModel.nMinusOneStatistics.foreach(t => out.println(s"${t._1} :=: ${t._2}"))
    }
  }

  def readFrom(filename: String): NGrammModel = {
    val file = Source.fromFile(filename).getLines().toList
    val n = file.head.toInt
    val dic1Size = file(1).toInt
    val dic2Size = file(2).toInt
    val dict1 = file.drop(3).take(dic1Size).map(_.split(" :=: ")).map(s => s(0) -> s(1).toInt).toMap
    val dict2 = file.drop(3 + dic1Size).take(dic2Size).map(_.split(" :=: ")).map(s => s(0) -> s(1).toInt).toMap

    new NGrammModel(n, dict1, dict2)
  }
}
