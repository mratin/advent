package se.apogo.advent17

import scala.io.Source

object Day2 extends App {
  val inputLines: Seq[String] = Source.fromResource("se/apogo/advent17/input02.txt").getLines.toSeq

  def lineSum(line: String): Int = {
    val parsedLine: Seq[Int] = line.split("\\s").map(Integer.parseInt)

    val min = parsedLine.min
    val max = parsedLine.max

    max - min
  }

  println(inputLines.map(lineSum).sum)
}

object Day2_2 extends App {
  val inputLines: Seq[String] = Source.fromResource("se/apogo/advent17/input02.txt").getLines.toSeq

  def lineValue(line: String): Int = {
    val parsedLine: IndexedSeq[Int] = line.split("\\s").map(Integer.parseInt)

    val wholeDivisions = for {
      i <- 0 to parsedLine.size - 2
      j <- i+1 until parsedLine.size
      a = parsedLine(i)
      b = parsedLine(j)
      max = math.max(a,b)
      min = math.min(a,b)
      if max % min == 0
    } yield {
      max / min
    }

    assert(wholeDivisions.size == 1)

    wholeDivisions.head
  }

  println(inputLines.map(lineValue).sum)
}
