import common.loadPackets

val input: List[String] = loadPackets(List("day05.txt"))
val park: List[List[Int]] = input.map(_.codePoints().toArray.toList)
val width: Int = park.head.size
val poo: Int = "💩".codePointAt(0)

case class Point(row: Int = 0, col: Int = 0) {
  val isInPark: Boolean = park.indices.contains(row)
  def isPoo: Boolean = park(row)(col) == poo
  def step: Point = copy(row = row + 1, col = (col + 2) % width)
}

LazyList.iterate(Point())(_.step)
  .takeWhile(_.isInPark)
  .count(_.isPoo)