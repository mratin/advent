package se.apogo.advent16

import scala.io.Source

object Day9_1 extends App {
  val input: String = Source.fromResource("se/apogo/advent16/input9.txt").getLines.toSeq.head

  def decompress(s: String): String = {
    val pattern = """\((\d+)+x(\d+)+\)""".r

    pattern.findFirstMatchIn(s) match {
      case None => s
      case Some(m) =>
        val numberOfCharacters = m.group(1).toInt
        val times = m.group(2).toInt
        val (toRepeat, rest) = m.after.toString.splitAt(numberOfCharacters)
        val expanded = Seq.fill(times)(toRepeat).mkString
        m.before.toString + expanded + decompress(rest)
    }
  }

  val result = decompress(input)
  println(result.length)
}

object Day9_2 extends App {
  val input: String = Source.fromResource("se/apogo/advent16/input9.txt").getLines.toSeq.head

  def decompressedLength(s: String): Long = {
    val pattern = """\((\d+)+x(\d+)+\)""".r

    pattern.findFirstMatchIn(s) match {
      case None => s.length
      case Some(m) =>
        val numberOfCharacters = m.group(1).toLong
        val times = m.group(2).toLong
        val (toRepeat, rest) = m.after.toString.splitAt(numberOfCharacters.toInt)
        val expandedLength = decompressedLength(toRepeat) * times
        m.before.toString.length + expandedLength + decompressedLength(rest)
    }
  }

  val result = decompressedLength(input)
  println(result)
}
