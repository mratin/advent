package se.apogo.advent17

import scala.io.Source

object Day4_1 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input04.txt").getLines.toIndexedSeq

  val result: Int = input.count(line => {
    val split: Array[String] = line.split("\\s").map(_.trim)
    split.toSet.size == split.length
  })

  println(result)
}


object Day4_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input04.txt").getLines.toIndexedSeq

  val result: Int = input.count(line => {
    val split: Array[String] = line.split("\\s").map(_.trim)

    def perm(s: String): Seq[String] = s.permutations.toSeq

    (for {i <- split.indices } yield {
      val perms = perm(split(i))
      for { j <- split.indices if i != j } yield {
        perms.contains(split(j))
      }
    }).forall(_.forall(!_))
  })

  println(result)
}


/**
  * This would have been a much better solution to part 2 (only a small change,
  * compared to part 1), but I didn't think of it when I faced the problem. :)
  */
object Day4_2b extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input04.txt").getLines.toIndexedSeq

  val result: Int = input.count(line => {
    val split: Array[String] = line.split("\\s").map(_.trim)
    split.map(_.sorted).toSet.size == split.length
  })

  println(result)
}
