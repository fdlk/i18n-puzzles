import common.loadPackets

import java.nio.charset.Charset
import java.util.HexFormat

val hex = HexFormat.of()
val input = loadPackets(List("day13.txt"))

def parseHex(line: String): (Array[Byte], List[Charset]) = line match {
  case s"feff${bytes}" => (hex.parseHex(bytes), List(Charset.forName("UTF-16BE")))
  case s"fffe${bytes}" => (hex.parseHex(bytes), List(Charset.forName("UTF-16LE")))
  case s"efbbbf${bytes}" => (hex.parseHex(bytes), List(Charset.forName("UTF-8")))
  case _ =>
    val bytes = hex.parseHex(line)
    bytes.indexOf(0) match {
      case -1 => (bytes, List(Charset.forName("UTF-8"), Charset.forName("iso-8859-1")))
      case i if i % 2 == 0 => (bytes, List(Charset.forName("UTF-16BE")))
      case i if i % 2 == 1 => (bytes, List(Charset.forName("UTF-16LE")))
    }
}

def parseBytes(line: Array[Byte], charsets: List[Charset]): String =
  charsets.map(new String(line, _))
    .find(_.forall(_.isLetter))
    .get

val words = input.takeWhile(_.nonEmpty).map(parseHex).map(parseBytes)
val puzzle = input.dropWhile(_.nonEmpty).tail.map(_.trim)

puzzle.map(pattern => words.indexWhere(pattern.r.matches))
  .map(_ + 1)
  .sum