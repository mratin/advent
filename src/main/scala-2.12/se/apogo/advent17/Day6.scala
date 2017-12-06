package se.apogo.advent17

import scala.io.Source

object Day6_1 extends App {
  val input: IndexedSeq[Int] = Source.fromResource("se/apogo/advent17/input06.txt").
    getLines.toSeq.head.split("\\s").map(Integer.parseInt)

  def redist(mem: IndexedSeq[Int], left: Int, i: Int): IndexedSeq[Int] = {
    if (left == 0) mem else {
      if (i == mem.size) redist(mem, left, 0) else {
        redist(mem.updated(i, mem(i)+1), left-1, i+1)
      }
    }
  }

  def loop(n: Int, mem: IndexedSeq[Int], seen: Set[IndexedSeq[Int]]): Int = {
    if (seen.contains(mem)) n else {
      val max = mem.max
      val maxIndex = mem.indexWhere(_ == max)

      val nextMem = redist(mem.updated(maxIndex, 0), max, maxIndex+1)

      loop(n+1, nextMem, seen + mem)
    }
  }

  val result = loop(0, input, Set.empty)

  println(result)
}

object Day6_2 extends App {
  val input: IndexedSeq[Int] = Source.fromResource("se/apogo/advent17/input06.txt").
    getLines.toSeq.head.split("\\s").map(Integer.parseInt)

  def redist(mem: IndexedSeq[Int], left: Int, i: Int): IndexedSeq[Int] = {
    if (left == 0) mem else {
      if (i == mem.size) redist(mem, left, 0) else {
        redist(mem.updated(i, mem(i)+1), left-1, i+1)
      }
    }
  }

  def loop(n: Int, mem: IndexedSeq[Int], seen: Set[IndexedSeq[Int]]): (Int, IndexedSeq[Int]) = {
    if (seen.contains(mem)) (n, mem) else {
      val max = mem.max
      val maxIndex = mem.indexWhere(_ == max)

      val nextMem = redist(mem.updated(maxIndex, 0), max, maxIndex+1)

      loop(n+1, nextMem, seen + mem)
    }
  }

  val infLoop = loop(0, input, Set.empty)._2

  val result = loop(0, infLoop, Set.empty)

  println(result)
}



