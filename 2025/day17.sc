import common.loadPackets

import java.util.HexFormat
import scala.annotation.tailrec

val input = loadPackets(List("day17.txt")).appended("")
val hex = HexFormat.of()
val (chunks, _) = input.foldLeft((Nil, Nil): (List[List[String]], List[String]))({
  case ((chunks, chunk), line) if line.isEmpty => (chunks ::: List(chunk), List())
  case ((chunks, chunk), line) => (chunks, chunk.appended(line))
})

def typeOfCodepoint(value: String) = value match {
  case s"0${rest}" => 0
  case s"110${rest}" => 1
  case s"1110${rest}" => 2
  case s"11110${rest}" => 3
  case s"10${rest}" => -1
}

def parseBits(line: String) =
  line.grouped(2)
    .toList
    .map(byte => BigInt(byte, 16).toString(2).reverse.padTo(8, '0').reverse)

def rightSignature(types: List[Int]): Int =
  val minusOnes = types.takeWhile(_ == -1).length
  types.take(minusOnes + 1).sum

def signature(line: String): List[Int] =
  val types = parseBits(line).map(typeOfCodepoint)
  List(types.takeWhile(_ == -1).length, rightSignature(types.reverse))

case class MapPiece(lines: List[String]) {
  val List(left, right) = lines.map(signature).transpose
}

val pieces = chunks.map(MapPiece.apply)
val left = pieces.filter(_.left.forall(_ == 0))
val rest = pieces.toSet.diff(left.toSet).toList

def solveColumn(edge: List[Int], pieces: Set[MapPiece], soFar: List[MapPiece] = Nil): Option[List[MapPiece]] =
  if edge.isEmpty then Some(soFar)
  else
    pieces.find(piece => edge.startsWith(piece.left))
      .flatMap(piece => solveColumn(edge.drop(piece.lines.size), pieces.diff(Set(piece)), soFar.appended(piece)))

def solve(soFar: List[List[MapPiece]], edge: List[Int], rest: Set[MapPiece]): Option[List[List[MapPiece]]] =
  if rest.isEmpty then Some(soFar)
  else
    solveColumn(edge, rest)
      .flatMap(foundCol => solve(soFar.appended(foundCol), foundCol.flatMap(_.right), rest.diff(foundCol.toSet)))

val stitched = left.permutations
  .flatMap(pieces => solve(List(pieces), pieces.flatMap(_.right), rest.toSet))
  .toList.head

val map = stitched.map(_.flatMap(_.lines)).transpose.map(_.flatMap(hex.parseHex).toArray).map(new String(_, "UTF-8"))
val row = map.indexWhere(_.contains("╳"))
val col = map(row).codePoints.toArray.indexOf('╳')
row * col