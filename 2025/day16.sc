import common.loadPackets

import java.lang.Character.UnicodeBlock
import scala.io.Codec

type Puzzle = List[String]
//val input = loadPackets(List("day16-with-border.txt"), Codec("CP437"))
//val puzzleWithBorder: Puzzle = input.map(_.chars().map({
//    case c if UnicodeBlock.of(c) == UnicodeBlock.BOX_DRAWING => c
//    case _ => ' '
//  }).mapToObj(_.toChar).toList.toArray.mkString)
// manually chop off the border
val initialPuzzle = loadPackets(List("day16.txt"))

val rows = initialPuzzle.indices
val cols = initialPuzzle.head.indices

// key: pipes rotated clockwise
// number of pipe ends leading up, right, down, left
val pipes = Map(
  " " -> "0",
  "┌┐┘└" -> "0110",
  "╔╗╝╚" -> "0220",
  "┬┤┴├" -> "0111",
  "╥╡╨╞" -> "0121",
  "╤╢╧╟" -> "0212",
  "╦╣╩╠" -> "0222",
  "│─" -> "1010",
  "║═" -> "2020",
  "╪╫" -> "1212",
  "┼" -> "1111"
)

enum Direction:
  case Up, Right, Down, Left
  def rotate: Direction = this match {
    case Up => Right
    case Right => Down
    case Down => Left
    case Left => Up
  }
  def opposite: Direction = rotate.rotate

case class Point(row: Int, col: Int) {
  val isOnMap = rows.contains(row) && cols.contains(col)
  def move(direction: Direction): Point = direction match {
    case Direction.Up => copy(row = row - 1)
    case Direction.Left => copy(col = col - 1)
    case Direction.Right => copy(col = col + 1)
    case Direction.Down => copy(row = row + 1)
  }
}

def charAt(puzzle: Puzzle, p: Point): Char =
  if p.isOnMap
  then puzzle(p.row)(p.col)
  else ' '

case class State(puzzle: Puzzle, open: Set[Point], closed: Set[Point], numRotations: Int = 0) {
  /**
   * Gives the options, of how many pipes can exit p in direction d
   */
  def options(p: Point, d: Direction): Set[Char] =
    val c = charAt(puzzle, p)
    val (rotation, edges) = pipes.find(_._1.contains(c)).get
    if closed.contains(p)
    then
      val edgeIndex = (Direction.values.indexOf(d) - rotation.indexOf(c) + edges.length) % edges.length
      Set(edges(edgeIndex))
    else
      edges.toSet

  /**
   * If there is only one way to rotate the pipe at point p,
   * returns the correct rotation plus how many clockwise rotations it takes to get there.
   */
  def examine(p: Point): Option[(Point, Char, Int)] =
    val neighbors = Direction.values
      .map(d => options(p.move(d), d.opposite))
      .toList
    val c = charAt(puzzle, p)
    val (rotations, edges) = pipes.find(_._1.contains(c)).get
    val possibilities = rotations.zipWithIndex.filter({
      case (rotC, index) =>
        val rotatedEdges = edges.takeRight(index) + edges.dropRight(index)
        (rotatedEdges zip neighbors).forall((actual, possible) => possible.contains(actual))
    })
    if possibilities.size == 1
    then possibilities.head match {
      case (rotated, index) => Some((p, rotated, (index - rotations.indexOf(c) + rotations.length) % rotations.length))
    }
    else None

  def next =
    val deduced = open.flatMap(examine)
    val newPipes: Map[Point, Char] = deduced.map((p,c,_) => p -> c).toMap
    val newPuzzle: Puzzle = rows.map(row => cols.map(col => Point(row,col)).map(p =>
      newPipes.getOrElse(p, charAt(puzzle, p))).mkString).toList
    State(
      puzzle = newPuzzle,
      open = open.diff(newPipes.keys.toSet),
      closed = closed.union(newPipes.keys.toSet),
      numRotations = numRotations + deduced.toList.map(_._3).sum
    )
}

val points = rows.flatMap(row => cols.map(col => Point(row, col))).toSet
val (open, closed) = points.partition(charAt(initialPuzzle, _) != ' ')
val state: State = State(puzzle = initialPuzzle, open = open, closed = closed)

LazyList.iterate(state)(_.next).map(_.numRotations).grouped(2).map(_.toSet).find(_.size == 1).get
