package se.apogo.advent18

object Day14 extends App {
  val input = 540391

  def cook(recipes: IndexedSeq[Int], e1: Int, e2: Int): Seq[Int] = {
    if (recipes.size >= input + 10) recipes else {
      val sum = recipes(e1) + recipes(e2)
      val created: Seq[Int] = sum.toString.map(_.toString).map(Integer.parseInt)
      val newRecipes = recipes ++ created
      def shift(from: Int) = (from + newRecipes(from) + 1) % newRecipes.size
      cook(newRecipes, shift(e1), shift(e2))
    }
  }

  val result = cook(IndexedSeq(3,7), 0, 1)
  println(result.drop(input).mkString)
}

object Day14_2 extends App {
  val input = "540391"

  def cook(e1: Int, e2: Int, end: String, m: Map[Int, Int], i: Int): Int = {
    if (end == input) i-input.length else {
      val sum = m(e1) + m(e2)
      val created: String = sum.toString
      val newRecipes = if (sum >= 10) m.updated(i, sum / 10).updated(i+1, sum % 10) else m.updated(i, sum)
      val smallEnd = (end :+ created.head).takeRight(input.length)
      if (smallEnd == input) i-input.length+1 else {
        val newEnd = (end ++ created).takeRight(input.length)
        def shift(from: Int) = (from + newRecipes(from) + 1) % newRecipes.size
        cook(shift(e1), shift(e2), newEnd, newRecipes, i + created.length)
      }
    }
  }

  val result = cook(0, 1, "37", Map(0 -> 3, 1 -> 7), 2)
  println(result)
}
