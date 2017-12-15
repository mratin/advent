package se.apogo.advent17

object Day15_1 extends App {
  case class Gen(factor: Long, number: Long) {
    def next: Gen = copy(number = number * factor % 2147483647)
  }

  def run(a: Gen, b: Gen, count: Int, times: Int): Int = {
    if (times == 0) count else {
      val newCount = if ((a.number.toInt & 0xFFFF) == (b.number.toInt & 0xFFFF)) count+1 else count
      run(a.next, b.next, newCount, times - 1)
    }
  }

  val result = run(Gen(16807, 699), Gen(48271, 124), 0, 40000000)

  println(result)
}


object Day15_2 extends App {
  case class Gen(factor: Long, number: Long, mult: Long) {
    def next: Gen = {
      val nextGen = copy(number = number * factor % 2147483647)
      if (nextGen.number % mult == 0) {
        nextGen
      } else {
        nextGen.next
      }
    }
  }

  def run(a: Gen, b: Gen, count: Int, times: Int): Int = {
    if (times == 0) count else {
      val newCount = if ((a.number.toInt & 0xFFFF) == (b.number.toInt & 0xFFFF)) count+1 else count
      run(a.next, b.next, newCount, times - 1)
    }
  }

  val result = run(Gen(16807, 699, 4), Gen(48271, 124, 8), 0, 5000000)

  println(result)
}
