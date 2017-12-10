package se.apogo.advent17

import scala.io.Source

object Day10_1 extends App {
  val input: List[Int] = Source.fromResource("se/apogo/advent17/input10.txt").
    getLines.toSeq.head.split(",").map(_.trim).map(Integer.parseInt).toList

  def loop(list: IndexedSeq[Int], current: Int, skip: Int, input: List[Int]): Seq[Int] = {
    input match {
      case Nil => list
      case l::ls => {
        val wrap = math.max(current+l - list.size,0)
        val list2 = list ++ list
        val reverseSlice = list2.slice(current, current+l).reverse
        val list3 = list.take(current) ++ reverseSlice ++ list2.drop(current+l)
        val list4 = list3.slice(list.size, list.size + wrap) ++ list3.drop(wrap)
        val nextCurrent = (current+l+skip) % list.size
        loop(list4.take(list.size), nextCurrent, skip+1, ls)
      }
    }
  }

  val result = loop(0 to 255, 0, 0, input)

  println(result(0)*result(1))
}


object Day10_2 extends App {
  val input: List[Int] = Source.fromResource("se/apogo/advent17/input10.txt").
    getLines.toSeq.head.trim.map(_.toByte.toInt).toList ++
    List(17, 31, 73, 47, 23)

  def loop(list: IndexedSeq[Int], current: Int, skip: Int, input: List[Int]): Seq[Int] = {
    input match {
      case Nil => list
      case l::ls => {
        val wrap = math.max(current+l - list.size,0)
        val list2 = list ++ list
        val reverseSlice = list2.slice(current, current+l).reverse
        val list3 = list.take(current) ++ reverseSlice ++ list2.drop(current+l)
        val list4 = list3.slice(list.size, list.size + wrap) ++ list3.drop(wrap)
        val nextCurrent = (current+l+skip) % list.size
        loop(list4.take(list.size), nextCurrent, skip+1, ls)
      }
    }
  }

  val lengths: Seq[Int] = (1 to 64).flatMap(_ => input)
  val sparseHash = loop(0 to 255, 0, 0, lengths.toList)
  val denseHash = sparseHash.sliding(16,16).toSeq.map(slice => slice.reduce(_ ^ _))

  def toHex(i: Int): String = {
    val hex = Integer.toHexString(i)
    Seq.fill(2 - hex.length)("0").mkString + hex
  }

  val hexHash = denseHash.map(toHex).mkString

  println(hexHash)
}
