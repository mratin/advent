package se.apogo.advent17

object Day17_1 extends App {
  val input = 303

  def loop(buffer: Seq[Int], position: Int, value: Int, to: Int): Seq[Int] = {
    if (value > to) {
      buffer
    } else {
      val i = (position + input) % buffer.size + 1
      val next = buffer.take(i) ++ Seq(value) ++ buffer.drop(i)
      loop(next, i, value+1, to)
    }
  }

  val buffer = loop(Seq(0), 0, 1, 2017)
  val result = buffer((buffer.indexOf(2017)+1) % buffer.size)
  println(result)
}


object Day17_2 extends App {
  val input = 303

  def loop(afterZero: Int, afterZeroPosition: Int,
           position: Int, value: Int, to: Int): Int = {
    if (value > to) {
      afterZero
    } else {
      val i = (position + input) % value + 1
      if (i == afterZeroPosition) {
        loop(value, i, i, value+1, to)
      } else {
        loop(afterZero, afterZeroPosition, i, value+1, to)
      }
    }
  }

  val result = loop(0, 1, 0, 1, 50000000)
  println(result)
}
