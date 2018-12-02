package se.apogo.advent18

import scala.io.Source

object Day01 extends App {
  val changes = Source.fromResource("se/apogo/advent18/input01.txt").getLines.map(_.trim).map(Integer.parseInt)

  println(changes.sum)
}

object Day01_2 extends App {
  val changes: Seq[Int] = Source.fromResource("se/apogo/advent18/input01.txt").getLines.map(_.trim).map(Integer.parseInt).toSeq

  def freqs(cs: Seq[Int], f: Int, seen: Set[Int]): Int = {
    if (cs.isEmpty) freqs(changes, f, seen) else {
      val newF = f + cs.head
      if (seen.contains(newF)) newF else
        freqs(cs.tail, newF, seen + newF)
    }
  }

  println(freqs(Nil, 0, Set(0)))
}
