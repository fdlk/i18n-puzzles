import common.loadPackets

import java.util.HexFormat

val hex = HexFormat.of()
val input = loadPackets(List("day13.txt"))

def parseUtf8OrIsoLatin(line: Array[Byte]): String =
  List("iso-8859-1", "UTF-8")
    .map(new String(line, _))
    .find(l => l.forall(c => c.isLetter))
    .get

def parse(line: Array[Byte]): String =
  if line(0) == -2 && line(1) == -1
  then new String(line.drop(2), "UTF-16BE")
  else if line(0) == -1 && line(1) == -2
    then new String(line.drop(2), "UTF-16LE")
  else if line(0) == -17 && line(1) == -69 && line(2) == -65
    then new String(line.drop(3), "UTF-8")
  else line.indexOf(0) match {
    case -1 => parseUtf8OrIsoLatin(line)
    case i if i % 2 == 0 => new String(line, "UTF-16BE")
    case i if i % 2 == 1 => new String(line, "UTF-16LE")
  }


val words = input.takeWhile(_.nonEmpty).map(hex.parseHex).map(parse)
val puzzle = input.dropWhile(_.nonEmpty).tail.map(_.trim)

puzzle.map(pattern => words.indexWhere(pattern.r.matches))
  .map(_ + 1)
  .sum