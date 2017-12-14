package se.apogo.advent17

import scala.io.Source

object Day14_1 extends App {
  val input: String = Source.fromResource("se/apogo/advent17/input14.txt").getLines.toSeq.head

  def knotHash(string: String): String = {
    def loop(list: IndexedSeq[Int], current: Int, skip: Int, input: List[Int]): Seq[Int] = {
      input match {
        case Nil => list
        case l::ls =>
          val wrap = math.max(current+l - list.size,0)
          val list2 = list ++ list
          val reverseSlice = list2.slice(current, current+l).reverse
          val list3 = list.take(current) ++ reverseSlice ++ list2.drop(current+l)
          val list4 = list3.slice(list.size, list.size + wrap) ++ list3.drop(wrap)
          val nextCurrent = (current+l+skip) % list.size
          loop(list4.take(list.size), nextCurrent, skip+1, ls)
      }
    }

    val bytes = string.map(_.toByte.toInt).toList ++ List(17, 31, 73, 47, 23)
    val lengths: Seq[Int] = (1 to 64).flatMap(_ => bytes)
    val sparseHash = loop(0 to 255, 0, 0, lengths.toList)
    val denseHash = sparseHash.sliding(16,16).map(_.reduce(_ ^ _))

    denseHash.map(d => "%2s".format(Integer.toString(d, 16)).replace(' ', '0')).mkString
  }

  def toBinary(hash: String): String =
    hash.map(hex => "%4s".format(Integer.parseInt(hex.toString, 16).toBinaryString).replace(' ', '0')).mkString

  val hashes: Seq[String] = (0 to 127).map(input + "-" + _).map(knotHash)
  val result: Int = hashes.map(toBinary).map(_.count(_ == '1')).sum

  println(result)
}


object Day14_2 extends App {
  val input: String = Source.fromResource("se/apogo/advent17/input14.txt").getLines.toSeq.head

  def knotHash(string: String): String = {
    def loop(list: IndexedSeq[Int], current: Int, skip: Int, input: List[Int]): Seq[Int] = {
      input match {
        case Nil => list
        case l::ls =>
          val wrap = math.max(current+l - list.size,0)
          val list2 = list ++ list
          val reverseSlice = list2.slice(current, current+l).reverse
          val list3 = list.take(current) ++ reverseSlice ++ list2.drop(current+l)
          val list4 = list3.slice(list.size, list.size + wrap) ++ list3.drop(wrap)
          val nextCurrent = (current+l+skip) % list.size
          loop(list4.take(list.size), nextCurrent, skip+1, ls)
      }
    }

    val bytes = string.map(_.toByte.toInt).toList ++ List(17, 31, 73, 47, 23)
    val lengths: Seq[Int] = (1 to 64).flatMap(_ => bytes)
    val sparseHash = loop(0 to 255, 0, 0, lengths.toList)
    val denseHash = sparseHash.sliding(16,16).map(_.reduce(_ ^ _))

    denseHash.map(d => "%2s".format(Integer.toString(d, 16)).replace(' ', '0')).mkString
  }

  def toBinary(hash: String): String =
    hash.map(hex => "%4s".format(Integer.parseInt(hex.toString, 16).toBinaryString).replace(' ', '0')).mkString

  case class Square(i: Int, j: Int)
  case class Disk(used: Set[Square]) {
    def isUsed(square: Square) = {
      def inside(x: Int): Boolean = 0 <= x && x < 128
      used.contains(square) && inside(square.i) && inside(square.j)
    }

    def neighbours(square: Square) = {
      Set(square.copy(i = square.i-1), square.copy(i = square.i+1),
        square.copy(j = square.j-1), square.copy(j = square.j+1)).filter(isUsed)
    }
  }

  def visit(disk: Disk, toVisit: List[Square], visited: Set[Square]): Set[Square] = {
    toVisit match {
      case Nil => visited
      case node::ns if visited.contains(node) => visit(disk, ns, visited)
      case node::ns =>
        val unvisitedNeighbours = disk.neighbours(node) -- visited
        visit(disk, ns ++ unvisitedNeighbours.toList, visited + node)
    }
  }

  val hashes: Seq[String] = (0 to 127).map(input + "-" + _).map(knotHash)
  val binaries: Seq[String] = hashes.map(toBinary)

  val disk = Disk((for {
    i <- 0 to 127
    j <- 0 to 127
    if binaries(i)(j) == '1'
  } yield {
    Square(i,j)
  }).toSet)

  val result = disk.used.groupBy(s => visit(disk, List(s), Set.empty))

  println(result.size)
}
