import common.loadPackets

import java.lang.Character.UnicodeBlock
import scala.io.Codec

val input = loadPackets(List("day16.txt"), Codec("CP437"))
  .map(_.chars().map({
    case c if UnicodeBlock.of(c) == UnicodeBlock.BOX_DRAWING => c
    case _ => ' '
  }).mapToObj(_.toChar).toList.toArray.mkString)
input.mkString("\n")

val rotations = List("┌┐┘└", "╔╗╝╚", "┬┤┴├", "╥╡╨╞", "╤╢╧╟", "╦╣╩╠", "│─", "═║", "╪╫", "┼")

val puzzle = loadPackets(List("day16.txt"))
val solved = loadPackets(List("day16-solved.txt"))

def countRotations(c: Char, cs: Char): Int =
  val group: String = rotations.find(_.contains(c)).get
  val i = group.indexOf(c)
  val j = group.indexOf(cs)
  (j - i + group.length) % group.length

(puzzle zip solved)
  .map((line, lineSolved) => (line zip lineSolved)
    .map({
      case (c, cs) if c == cs => 0
      case (c, cs) if rotations.exists(_.contains(c)) => countRotations(c, cs)
    }).sum
  ).sum