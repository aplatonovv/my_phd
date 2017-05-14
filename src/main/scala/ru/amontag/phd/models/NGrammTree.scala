package ru.amontag.phd.models

/**
  * Created by amontag on 14.05.2017.
  */
object NGrammTree {
  def build(models: Map[Int, NGrammModel])(sequence: String): NGrammTree = {
    val allPossibleFragmentation = models.flatMap({
      case (n, model) =>
        model.split(sequence).map(seq => seq -> model.estimateNGramm(seq))
    }).toSeq.sortBy(t => (-t._2, -t._1.size))

    new NGrammTree(Seq.empty)
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
