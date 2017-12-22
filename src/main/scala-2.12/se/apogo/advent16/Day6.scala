package se.apogo.advent16

import scala.collection.immutable.{IndexedSeq, Seq}
import scala.io.Source

object Day6_1 extends App {
  val input = Source.fromResource("se/apogo/advent16/input6.txt").getLines.toSeq

  val columns: Seq[IndexedSeq[Char]] = input.head.indices.map(i => {
    input.indices.map(j => input(j)(i))
  })

  val result = columns.map(col => col.map(c => c -> col.count(_ == c)).maxBy(_._2)._1)

  println(result.mkString)
}

object Day6_2 extends App {
  val input = Source.fromResource("se/apogo/advent16/input6.txt").getLines.toSeq

  val columns: Seq[IndexedSeq[Char]] = input.head.indices.map(i => {
    input.indices.map(j => input(j)(i))
  })

  val result = columns.map(col => col.map(c => c -> col.count(_ == c)).minBy(_._2)._1)

  println(result.mkString)
}
