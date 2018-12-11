package se.apogo.advent18

object Day11 extends App {
  def value(x: Int, y: Int) = {
    val rackId = x + 10
    (((rackId * y + 9445) * rackId) / 100) % 10 - 5
  }

  val result = {
    (for {
      x <- 1 to 300
      y <- 1 to 300
    } yield {
      (x,y) -> (for (i <- 0 to 2; j <- 0 to 2; if i <= 300 && j <= 300) yield value(x + i, y + j)).sum
    }).toMap
  }

  println(result.maxBy(_._2))
}

object Day11_2 extends App {
  def compute(x: Int, y: Int) = {
    val rackId = x + 10
    (((rackId * y + 9445) * rackId) / 100) % 10 - 5
  }

  val value: Map[(Int, Int), Int] = (for (i <- 1 to 300; j <- 1 to 300) yield (i,j) -> compute(i,j)).toMap

  val result = (for {
      x <- (1 to 300).par
      y <- (1 to 300).par
    } yield {
      def sizeSearch(sizes: Seq[Int], prevSum: Int, acc: Set[(Int,Int,Int,Int)]): Set[(Int,Int,Int,Int)] = {
        if (sizes.isEmpty) acc else {
          val size = sizes.head
          val xs = (for (i <- 0 until size) yield value(x + i, y + size-1)).sum
          val ys = (for (j <- 0 until size) yield value(x + size-1, y + j)).sum
          val v = prevSum + xs + ys
          sizeSearch(sizes.tail, v, acc + ((x,y,size,v)))
        }
      }
    sizeSearch(1 to (301 - math.max(x, y)), 0, Set())
  }).flatten.maxBy(_._4)

  println(result)
}
