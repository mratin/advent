package se.apogo.advent18

object Day09 extends App {
  val players = 464
  val last = 70918

  def play(pos: Int, circle: Seq[Int], i: Int, playerScores: Map[Int,Int]): Int = {
    if (i > last) playerScores.values.max else {
      if (i % 23 != 0) {
        val newPos = (pos + 2) % circle.size
        val (ccw, cw) = circle.splitAt(newPos)
        val newCircle = (ccw :+ i) ++ cw
        play(newPos, newCircle, i+1, playerScores)
      } else {
        val removePos = if (pos < 7) circle.size - (7 - pos) else pos - 7
        val removed = circle(removePos)
        val player = i % players
        val newScores = playerScores.updated(player, playerScores(player) + i + removed)
        val newCircle = circle.take(removePos) ++ circle.drop(removePos+1)
        play(removePos, newCircle, i+1, newScores)
      }
    }
  }

  println(play(0, Seq(0), 1, (0 until players).map(p => p -> 0).toMap))
}

object Day09_2 extends App {
  val players = 464
  val last = 7091800

  case class Circle(prev: Map[Int,Int], next: Map[Int,Int]) {
    def insert(value: Int, after: Int): Circle = Circle(
      prev.updated(value, after).updated(next(after), value),
      next.updated(after, value).updated(value, next(after)))

    def remove(value: Int): Circle = Circle(
      (prev - value).updated(next(value), prev(value)),
      (next - value).updated(prev(value), next(value)))

    def back(from: Int, n: Int): Int = if (n == 0) from else back(prev(from), n-1)
  }

  def play(circle: Circle, current: Int, i: Int, playerScores: Map[Int,Long]): Long = {
    val player = i % players
    if (i > last) playerScores.values.max else {
      if (i % 23 != 0) {
        play(circle.insert(i, circle.next(current)), i, i+1, playerScores)
      } else {
        val removed = circle.back(current, 7)
        val newScores = playerScores.updated(player, playerScores(player) + i + removed)
        play(circle.remove(removed), circle.next(removed), i+1, newScores)
      }
    }
  }

  println(play(Circle(Map(0 -> 0), Map(0 -> 0)), 0, 1, (0 until players).map(p => p -> 0L).toMap))
}
