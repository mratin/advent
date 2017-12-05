package se.apogo.advent17

import scala.io.Source

object Day5_1 extends App {
  val input: IndexedSeq[Int] = Source.fromResource("se/apogo/advent17/input05.txt").getLines.map(Integer.parseInt).toIndexedSeq

  def loop(n: Int, inst: IndexedSeq[Int], i: Int): Int = {
    if (!inst.indices.contains(i)) n else {
      val jump = inst(i)
      loop(n+1, inst.updated(i, jump+1), i+jump)
    }
  }

  val result = loop(0, input, 0)

  println(result)
}

object Day5_2 extends App {
  val input: IndexedSeq[Int] = Source.fromResource("se/apogo/advent17/input05.txt").getLines.map(Integer.parseInt).toIndexedSeq

  def loop(n: Int, inst: IndexedSeq[Int], i: Int): Int = {
    if (!inst.indices.contains(i)) n else {
      val jump = inst(i)
      loop(n+1, inst.updated(i, if (jump < 3) jump+1 else jump-1), i+jump)
    }
  }

  val result = loop(0, input, 0)

  println(result)
}



