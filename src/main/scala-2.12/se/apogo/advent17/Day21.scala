package se.apogo.advent17

import scala.io.Source

object Day21 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input21.txt").getLines.toSeq

  case class Vec(x: Int, y: Int) {
    def +(v: Vec) = Vec(x + v.x, y + v.y)
  }

  case class Grid(on: Set[Vec], size: Int) {
    lazy val flipX: Grid = copy(on = on.map(v => Vec(-v.x + size-1, v.y)))
    lazy val flipY: Grid = copy(on = on.map(v => Vec(v.x, -v.y + size-1)))
    lazy val rot90: Grid = copy(on = on.map(v => Vec(-v.y + size-1, v.x)))
    lazy val transforms: Set[Grid] = Set(rot90, rot90.rot90, rot90.rot90.rot90).flatMap(g => Set(g, g.flipX, g.flipY))
  }

  case class Rule(from: Grid, to: Grid) {
    def matches(grid: Grid): Boolean = grid.size == from.size && grid.transforms.contains(from)
  }

  def parseGrid(string: String): Grid = {
    val rows: Array[String] = string.split("/")
    val grid = rows.zipWithIndex.flatMap({case (row,y) => {
      row.zipWithIndex. flatMap({
        case ('#',x) => Set(Vec(x,y))
        case _ => Set[Vec]()
      })
    }}).toSet

    Grid(grid, rows.length)
  }

  def parseRule(line: String): Rule = {
    val Array(from, to) = line.split("=>").map(_.trim)
    Rule(parseGrid(from), parseGrid(to))
  }

  def splitBy(grid: Grid, n: Int): Seq[Seq[Grid]] = {
    for (x <- 0 until grid.size by n) yield {
      for (y <- 0 until grid.size by n) yield {
        val ons: Seq[Vec] = for {
          i <- x until x + n
          j <- y until y + n
          v = Vec(i, j)
          if grid.on.contains(v)
        } yield {
          v + Vec(-x,-y)
        }
        Grid(ons.toSet, n)
      }}
  }

  def splitGrid(grid: Grid): Seq[Seq[Grid]] = {
    if (grid.size % 2 == 0) {
      splitBy(grid, 2)
    } else {
      splitBy(grid, 3)
    }
  }

  def join(grids: Seq[Seq[Grid]]): Grid = {
    val size = grids.head.head.size
    require(grids.forall(_.head.size == size))

    val ons = for (i <- grids.indices) yield {
      val gridRow = grids(i)
      (for (j <- gridRow.indices) yield {
        val offset = Vec(i*size,j*size)
        val grid = gridRow(j)
        grid.on.map(_ + offset)
      }).flatten
    }

    Grid(ons.flatten.toSet, size * grids.size)
  }

  def run(rules: Seq[Rule])(grid: Grid): Grid = {
    join(splitGrid(grid).map(_.map(grid => rules.find(_.matches(grid)).get.to)))
  }

  val rules = input.map(parseRule)
  val grid = Grid(Set(Vec(1,0), Vec(2,1), Vec(0,2), Vec(1,2), Vec(2,2)), 3)

  val iterations = 5 // Part 2 uses the exact the same code except that iterations = 18
  val result = (1 to iterations).foldLeft(grid)((g,_) => run(rules)(g))
  println(result.on.size)
}

/**
  * Part 2 was solved using the same code as part 1, but the following solution
  * is a slightly cleaned implementation with somewhat better performance,
  * that was done after submission and some discussion with https://github.com/zayenz.
  * The original solution handled transforms in a unnecessarily complicated way.
  * I didn't realize at the time that I had forgotten the id transform, which lead to
  * some grids that didn't have a match. I "fixed" the problem by introducing a flip around the
  * y axis as well, not realizing that I could have just included the 'this' transform instead.
  * Another change is that all rules are pre-transformed and put in a map which
  * makes rule lookup a lot faster.
  * Lastly, I added a couple of `.par`s to make use of some parallelism when dealing with larger inputs.
  */
object Day21_postSubmit extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent17/input21.txt").getLines.toSeq

  case class Vec(x: Int, y: Int) {
    def +(v: Vec) = Vec(x + v.x, y + v.y)
  }

  case class Grid(on: Set[Vec], size: Int) {
    lazy val flip: Grid = copy(on = on.map(v => Vec(-v.x + size-1, v.y)))
    lazy val rot90: Grid = copy(on = on.map(v => Vec(-v.y + size-1, v.x)))
    lazy val transforms: Set[Grid] = Set(this, rot90, rot90.rot90, rot90.rot90.rot90).flatMap(g => Set(g, g.flip))
  }

  def parseGrid(string: String): Grid = {
    val rows: Array[String] = string.split("/")
    val grid = rows.zipWithIndex.flatMap({case (row,y) => {
      row.zipWithIndex.flatMap({
        case ('#',x) => Set(Vec(x,y))
        case _ => Set[Vec]()
      })
    }}).toSet

    Grid(grid, rows.length)
  }

  def parseRule(line: String): Map[Grid,Grid] = {
    val Array(f, t) = line.split("=>").map(_.trim)
    val to = parseGrid(t)
    parseGrid(f).transforms.map(_ -> to).toMap
  }

  def splitBy(grid: Grid, n: Int): Seq[Seq[Grid]] = {
    (for (x <- 0 until grid.size by n par) yield {
      (for (y <- 0 until grid.size by n par) yield {
        val ons: Seq[Vec] = for {
          i <- x until x + n
          j <- y until y + n
          v = Vec(i, j)
          if grid.on.contains(v)
        } yield {
          v + Vec(-x,-y)
        }
        Grid(ons.toSet, n)
      }).seq}).seq
  }

  def splitGrid(grid: Grid): Seq[Seq[Grid]] = {
    if (grid.size % 2 == 0) {
      splitBy(grid, 2)
    } else {
      splitBy(grid, 3)
    }
  }

  def join(grids: Seq[Seq[Grid]]): Grid = {
    val size = grids.head.head.size

    val ons = for (i <- grids.indices.par) yield {
      val gridRow = grids(i)
      (for (j <- gridRow.indices.par) yield {
        val offset = Vec(i*size,j*size)
        val grid = gridRow(j)
        grid.on.map(_ + offset)
      }).flatten
    }

    Grid(ons.flatten.toSet.seq, size * grids.size)
  }

  def run(rules: Map[Grid,Grid])(grid: Grid): Grid = {
    join(splitGrid(grid).par.map(_.par.map(rules).seq).seq)
  }

  val rules: Map[Grid,Grid] = input.flatMap(parseRule).toMap
  val grid = Grid(Set(Vec(1,0), Vec(2,1), Vec(0,2), Vec(1,2), Vec(2,2)), 3)

  val iterations = 18
  val result = (1 to iterations).foldLeft(grid)((g,_) => run(rules)(g))
  println(result.on.size)
}
