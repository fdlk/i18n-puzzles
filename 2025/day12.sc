import common.loadPackets

import java.text.Normalizer

val input = loadPackets(List("day12.txt"))

case class Person(first: String, last: String, phone: Long) {
  val lastWithoutPrefix = last.dropWhile(!_.isUpper)
  val order1: String = Normalizer.normalize(last, Normalizer.Form.NFKD)
    .filter(_.isLetter)
    .map(_.toUpper)
    .replaceAll("Æ", "AE")
    .replaceAll("Ø", "O")
  val order2: String = Normalizer.normalize(
      last.filter(_.isLetter)
        .map(_.toUpper)
        .replaceAll("Å", "a")
        .replaceAll("Æ", "b")
        .replaceAll("Ä", "b")
        .replaceAll("Ø", "c")
        .replaceAll("Ö", "c"),
      Normalizer.Form.NFKD)
    .filter(_.isLetter)
  val order3 = Normalizer.normalize(lastWithoutPrefix, Normalizer.Form.NFKD)
    .filter(_.isLetter)
    .map(_.toUpper)
    .replaceAll("Æ", "AE")
    .replaceAll("Ø", "O")
  val orderings = Array(order1, order2, order3)
}

val employees = input.map({
    case s"${last}, ${first}: ${phone}" => Person(first, last, phone.toLong)
  })

val middle = employees.length / 2 + employees.length % 2

(0 until 3).toList
  .map(i => employees.sortBy(_.orderings(i)).map(_.phone)(middle - 1))
  .product


