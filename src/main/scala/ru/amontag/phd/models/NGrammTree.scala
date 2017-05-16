package ru.amontag.phd.models

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  * Created by amontag on 14.05.2017.
  */
object NGrammTree {

  def build(models: Map[Int, NGrammModel])(sequence: String): NGrammTree = {
    val allPossibleFragmentations = models.flatMap({
      case (n, model) =>
        model.split(sequence).map(seq => seq -> model.estimateNGramm(seq))
    }).toSeq.sortBy(t => (-t._2, -t._1.size))

    val firstLevel = Level(allPossibleFragmentations.foldLeft(List[Either[String, Group]](Left(sequence)))((output, fragment) => {
      val result = output.flatMap(group => splitGroup(group, fragment))
      result
    }).map({
      case Left(string) => Group(string, 0.0)
      case Right(group) => group
    }))


    new NGrammTree(buildLevels(models, ArrayBuffer(firstLevel)))
  }

  private def splitGroup(group: Either[String, Group], fragment: (String, Double)): Seq[Either[String, Group]] = {
    group match {
      case Left(string) =>
        val split = string.split(fragment._1).filter(_.nonEmpty)
        val buffer = ArrayBuffer[Either[String, Group]]()

        if (split.isEmpty) {
          buffer += Right(Group(fragment._1, fragment._2))
        } else {
          if (split.size <= 1) {
            if (string.endsWith(fragment._1)) {
              buffer += Left(split(0))
              buffer += Right(Group(fragment._1, fragment._2))
            } else if (string.startsWith(fragment._1)) {
              buffer += Right(Group(fragment._1, fragment._2))
              buffer += Left(split(0))
            } else {
              buffer += Left(string)
            }
          } else if (fragment._1 == string) {
            buffer += Right(Group(fragment._1, fragment._2))
          } else {
            for (i <- 0 until split.size - 1 if split(i).nonEmpty) {
              buffer += Left(split(i))
              buffer += Right(Group(fragment._1, fragment._2))
            }

            buffer += Left(split.last)
          }
        }

        val result = buffer.toList
        result
      case Right(group) => Seq(Right(group))
    }
  }

  @tailrec
  private def buildLevels(models: Map[Int, NGrammModel], buffer: ArrayBuffer[Level]): Seq[Level] = {
    val lastLevel = buffer.last.groups
    if (lastLevel.size == 1) buffer.toSeq
    else {
      val allPossibleJoinings = lastLevel.zip(lastLevel.drop(1)) map {
        case (left, right) =>
          val maxProbability = models.values.map(model => model.estimateJoining(left.sentence, right.sentence)).max
          (left, right, maxProbability)
      }

      val (maxLeft, maxRight, probability) = allPossibleJoinings.maxBy(t => (t._3, (t._1.sentence + t._2.sentence).size))
      var lastGroupWasGlued = false

      val foundIndex = allPossibleJoinings.indexOf((maxLeft, maxRight, probability))
      val level = allPossibleJoinings.foldLeft(ArrayBuffer[Group]())((ans, join) => {
        if (join._1 == maxLeft && join._2 == maxRight) {
          lastGroupWasGlued = true
          val sentence = maxLeft.sentence + maxRight.sentence
          ans += Group(sentence, models.values.map(_.estimateSequence(sentence)).max)
        } else {
          if (!lastGroupWasGlued) {
            ans += join._1
          }
          lastGroupWasGlued = false
          ans
        }
      })

      if(foundIndex != allPossibleJoinings.size - 1) {
        level += allPossibleJoinings.last._2
      }
      buffer += Level(level)
      buildLevels(models, buffer)
    }
  }
}

class NGrammTree(val levels: Seq[Level]) {
  def apply(i: Int): Level = {
    levels(i)
  }

  def size(): Int = levels.size
}

case class Level(groups: Seq[Group])

case class Group(sentence: String, probability: Double)
