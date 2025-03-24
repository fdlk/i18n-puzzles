import common.loadPackets

import scala.annotation.tailrec
import fastparse.*
import ScriptWhitespace.*
import fastparse.Parsed.Success

val input = loadPackets(List("day18.txt"))

case object calculator {
  def eval(tree: (Double, Seq[(String, Double)])) =
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) => op match
        case "+" => left + right
        case "-" => left - right
        case "*" => left * right
        case "/" => left / right
    }
  def number[$: P]: P[Double] = P(CharIn("0-9").rep(1).!.map(_.toDouble))
  def parens[$: P]: P[Double] = P("(" ~/ addSub ~ ")")
  def factor[$: P]: P[Double] = P(number | parens)
  def divMul[$: P]: P[Double] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(eval)
  def addSub[$: P]: P[Double] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(eval)
  def expr[$: P]: P[Long] = P(addSub ~ End).map(_.round)
}

def bidi(line: String, level: Int = 0): List[Int] =
  if line.isEmpty then Nil else line.head match
    case '\u2067' | '⏴' => level :: bidi(line.tail, level + 1)
    case '\u2066' | '⏵' => level :: bidi(line.tail, level + 1)
    case '\u2069' | '⏶' => level - 1 :: bidi(line.tail, level - 1)
    case c if c.isDigit && level % 2 == 1 => level + 1 :: bidi(line.tail, level)
    case _ => level :: bidi(line.tail, level)

def reverse(stretch: String): String = stretch.reverseIterator.map {
  case ')' => '('
  case '(' => ')'
  case c => c
}.mkString

@tailrec
def reverseLevel(level: Int, line: String, levels: List[Int], from: Int = 0): String = {
  if level == 0 then return line
  if from >= line.length then return reverseLevel(level - 1, line, levels)
  val start = levels.indexWhere(_ >= level, from)
  if start == -1 then return reverseLevel(level - 1, line, levels)
    var end = levels.indexWhere(_ < level, start)
    if end == -1 then end = line.length
    val stretch = line.slice(start, end)
    val reversed = line.slice(0, start) + reverse(stretch) + line.slice(end, line.length)
    reverseLevel(level, reversed, levels, end)
}

def cleanMarks(value: String) = value.replaceAll("""[\u2067\u2066\u2069⏴⏵⏶]""", "")

def rex(line: String): Long =
  parse(cleanMarks(line), calculator.expr) match {
    case Success(value, _) => value
  }

def lynx(line: String): Long = {
  val levels = bidi(line)
  val flipped = reverseLevel(levels.max, line, levels)
  rex(flipped)
}

val totalDiff = input.map(line => (lynx(line)-rex(line)).abs).sum
