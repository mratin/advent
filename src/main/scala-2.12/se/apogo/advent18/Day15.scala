package se.apogo.advent18

import scala.io.Source

object Day15 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input15.txt").getLines.toIndexedSeq

  case class Vec(x: Int, y: Int) extends Comparable[Vec] {
    override def compareTo(o: Vec): Int = if (y.compareTo(o.y) == 0) x.compareTo(o.x) else y.compareTo(o.y)
    lazy val adjacent = Set(Vec(x+1, y), Vec(x-1, y), Vec(x, y+1), Vec(x, y-1))
  }

  case class Unit(id: Vec, unitType: Char, pos: Vec, hitPoints: Int, power: Int)

  case class Game(board: Map[Vec, Char], units: Set[Unit]) {
    def isEmptyPos(pos: Vec): Boolean = board.get(pos).contains('.') && units.forall(_.pos != pos)

    def gameOver = units.map(_.unitType).size == 1

    def path(from: Vec)(to: Vec): Option[Seq[Vec]] = {
      def search(toVisit: Seq[Vec], visited: Set[Vec], acc: Map[Vec, List[Vec]]): Option[Seq[Vec]] = {
        toVisit match {
          case Nil => None
          case pos::_ if pos == to => Some(acc(pos).reverse)
          case pos::ps if visited.contains(pos) => search(ps, visited, acc)
          case pos::ps =>
            val unvisitedNeighbours: Seq[Vec] = pos.adjacent.filter(isEmptyPos).toSeq.sorted.filterNot(visited.contains).filterNot(acc.isDefinedAt)
            search(ps ++ unvisitedNeighbours, visited + pos, unvisitedNeighbours.foldLeft(acc)((a, u) => a.updated(u, u::a(pos))))
        }
      }
      search(Seq(from), Set.empty, Map(from -> Nil))
    }
  }

  def turn(game: Game, unit: Unit, round: Int): Game = {
    val enemies: Set[Unit] = game.units.filter(_.unitType != unit.unitType)

    val emptyPosInRangeOfEnemies: Set[Vec] = enemies.flatMap(_.pos.adjacent).filter(pos => unit.pos == pos || game.isEmptyPos(pos))
    val paths: Map[Vec, Option[Seq[Vec]]] = emptyPosInRangeOfEnemies.map(p => p -> game.path(unit.pos)(p)).toMap
    val reachableEmptyPosInRangeOfEnemies: Set[Vec] = emptyPosInRangeOfEnemies.filter(e => paths(e).isDefined)

    val bestReachableEmptyPosInRange = reachableEmptyPosInRangeOfEnemies.toSeq.sorted.sortBy(p => paths(p).get.size)
    val (gameAfterMove, unitAfterMove) = bestReachableEmptyPosInRange.headOption match {
      case None =>
        (game, unit)
      case Some(bestReachableEmptyPos) =>
        paths(bestReachableEmptyPos).get.headOption match {
          case None => (game, unit) // no need to move
          case Some(moveTo) =>
            assert(unit.pos.adjacent.contains(moveTo))
            val movedUnit = unit.copy(pos = moveTo)
            (game.copy(units = (game.units - unit) + movedUnit), movedUnit)
        }
    }

    val enemiesInRange: Seq[Unit] = enemies.filter(e => unitAfterMove.pos.adjacent.contains(e.pos)).toSeq.sortBy(_.pos).sortBy(_.hitPoints)

    enemiesInRange.headOption match {
      case None => gameAfterMove // no-one to attack
      case Some(target) =>
        val attackedUnit = target.copy(hitPoints = target.hitPoints - unitAfterMove.power)
        if (attackedUnit.hitPoints <= 0)
          gameAfterMove.copy(units = gameAfterMove.units - target)
        else
          gameAfterMove.copy(units = (gameAfterMove.units - target) + attackedUnit)
    }
  }

  def play(game: Game, round: Int, unitIds: Seq[Vec]): (Game, Int) = {
    if (unitIds.isEmpty)
      if (game.gameOver) (game, round) else
        play(game, round+1, game.units.toSeq.sortBy(_.pos).map(_.id))
    else {
      if (game.gameOver) (game, round-1) else {
        val next = game.units.find(_.id == unitIds.head) match {
          case Some(unit) => turn(game, unit, round)
          case None => game // unit is dead
        }
        play(next, round, unitIds.tail)
      }
    }
  }

  val game: Game = {
    val parsed: Seq[(Vec, Char)] = for (y <- input.indices; row = input(y); x <- row.indices) yield (Vec(x,y), row(x))

    parsed.foldLeft(Game(Map(), Set()))((g, p) => {
      if ("GE".contains(p._2))
        g.copy(
          board = g.board.updated(p._1, '.'),
          units = g.units + Unit(p._1, p._2, p._1, 200, 3))
      else
        g.copy(board = g.board.updated(p._1, p._2))
    })
  }

  val (endGame, rounds) = play(game, 0, Nil)
  val result = endGame.units.toSeq.map(_.hitPoints).sum * rounds
  println(result)
}

object Day15_2 extends App {
  val input: Seq[String] = Source.fromResource("se/apogo/advent18/input15.txt").getLines.toIndexedSeq

  case class Vec(x: Int, y: Int) extends Comparable[Vec] {
    override def compareTo(o: Vec): Int = if (y.compareTo(o.y) == 0) x.compareTo(o.x) else y.compareTo(o.y)
    lazy val adjacent = Set(Vec(x+1, y), Vec(x-1, y), Vec(x, y+1), Vec(x, y-1))
  }

  case class Unit(id: Vec, unitType: Char, pos: Vec, hitPoints: Int, power: Int)

  case class Game(startElves: Int, board: Map[Vec, Char], units: Set[Unit]) {
    def isEmptyPos(pos: Vec): Boolean = board.get(pos).contains('.') && units.forall(_.pos != pos)

    def elvesWin = !units.exists(_.unitType == 'G')
    def gameOver = units.count(_.unitType == 'E') < startElves || elvesWin

    def path(from: Vec)(to: Vec): Option[Seq[Vec]] = {
      def search(toVisit: Seq[Vec], visited: Set[Vec], acc: Map[Vec, List[Vec]]): Option[Seq[Vec]] = {
        toVisit match {
          case Nil => None
          case pos::_ if pos == to => Some(acc(pos).reverse)
          case pos::ps if visited.contains(pos) => search(ps, visited, acc)
          case pos::ps =>
            val unvisitedNeighbours: Seq[Vec] = pos.adjacent.filter(isEmptyPos).toSeq.sorted.filterNot(visited.contains).filterNot(acc.isDefinedAt)
            search(ps ++ unvisitedNeighbours, visited + pos, unvisitedNeighbours.foldLeft(acc)((a, u) => a.updated(u, u::a(pos))))
        }
      }
      search(Seq(from), Set.empty, Map(from -> Nil))
    }
  }

  def turn(game: Game, unit: Unit): Game = {
    val enemies: Set[Unit] = game.units.filter(_.unitType != unit.unitType)

    val emptyPosInRangeOfEnemies: Set[Vec] = enemies.flatMap(_.pos.adjacent).filter(pos => unit.pos == pos || game.isEmptyPos(pos))
    val paths: Map[Vec, Option[Seq[Vec]]] = emptyPosInRangeOfEnemies.map(p => p -> game.path(unit.pos)(p)).toMap
    val reachableEmptyPosInRangeOfEnemies: Set[Vec] = emptyPosInRangeOfEnemies.filter(e => paths(e).isDefined)

    val bestReachableEmptyPosInRange = reachableEmptyPosInRangeOfEnemies.toSeq.sorted.sortBy(p => paths(p).get.size)
    val (gameAfterMove, unitAfterMove) = bestReachableEmptyPosInRange.headOption match {
      case None =>
        (game, unit)
      case Some(bestReachableEmptyPos) =>
        paths(bestReachableEmptyPos).get.headOption match {
          case None => (game, unit) // no need to move
          case Some(moveTo) =>
            assert(unit.pos.adjacent.contains(moveTo))
            val movedUnit = unit.copy(pos = moveTo)
            (game.copy(units = (game.units - unit) + movedUnit), movedUnit)
        }
    }

    val enemiesInRange: Seq[Unit] = enemies.filter(e => unitAfterMove.pos.adjacent.contains(e.pos)).toSeq.sortBy(_.pos).sortBy(_.hitPoints)

    enemiesInRange.headOption match {
      case None => gameAfterMove // no-one to attack
      case Some(target) =>
        val attackedUnit = target.copy(hitPoints = target.hitPoints - unitAfterMove.power)
        if (attackedUnit.hitPoints <= 0)
          gameAfterMove.copy(units = gameAfterMove.units - target)
        else
          gameAfterMove.copy(units = (gameAfterMove.units - target) + attackedUnit)
    }
  }

  def play(game: Game, round: Int, unitIds: Seq[Vec]): (Game, Int) = {
    if (unitIds.isEmpty)
      if (game.gameOver) (game, round) else
        play(game, round+1, game.units.toSeq.sortBy(_.pos).map(_.id))
    else {
      if (game.gameOver) (game, round-1) else {
        val next = game.units.find(_.id == unitIds.head) match {
          case Some(unit) => turn(game, unit)
          case None => game // unit is dead
        }
        play(next, round, unitIds.tail)
      }
    }
  }

  val game: Game = {
    val parsed: Seq[(Vec, Char)] = for (y <- input.indices; row = input(y); x <- row.indices) yield (Vec(x,y), row(x))

    parsed.foldLeft(Game(0, Map(), Set()))((g, p) => {
      if ("GE".contains(p._2))
        g.copy(
          board = g.board.updated(p._1, '.'),
          units = g.units + Unit(p._1, p._2, p._1, 200, 3),
          startElves = g.startElves + (if (p._2 == 'E') 1 else 0))
      else
        g.copy(board = g.board.updated(p._1, p._2))
    })
  }

  def powerGame(power: Int) =
    game.copy(units = game.units.map(u => if (u.unitType == 'E') u.copy(power = power) else u))

  def find(power: Int): (Game, Int) = {
    val end = play(powerGame(power), 0, Nil)
    if (end._1.elvesWin) end else find(power + 1)
  }

  val (endGame, rounds) = find(power = 4)

  val result = endGame.units.toSeq.map(_.hitPoints).sum * rounds
  println(result)
}
