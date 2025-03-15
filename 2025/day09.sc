import common.loadPackets

import java.time.{DateTimeException, LocalDate}
import java.time.format.ResolverStyle.STRICT
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField.YEAR

val input = loadPackets(List("day09.txt"))

val entries = input.map(_.split(":").map(_.trim)).flatMap({
  case Array(date, people) => people.split(", ").map(person => person -> date)
}).groupMap(_._1)(_._2).toVector

def createParser(format: DateTimeFormatter)(dates: List[String]) =
  try {
    dates.map(LocalDate.parse(_, format))
  } catch {
    case e: DateTimeException => List()
  }

val DMY = new DateTimeFormatterBuilder().appendPattern("dd-MM-").appendValueReduced(YEAR, 2, 2, 1920).toFormatter.withResolverStyle(STRICT)
val MDY = new DateTimeFormatterBuilder().appendPattern("MM-dd-").appendValueReduced(YEAR, 2, 2, 1920).toFormatter.withResolverStyle(STRICT)
val YMD = new DateTimeFormatterBuilder().appendValueReduced(YEAR, 2, 2, 1920).appendPattern("-MM-dd").toFormatter.withResolverStyle(STRICT)
val YDM = new DateTimeFormatterBuilder().appendValueReduced(YEAR, 2, 2, 1920).appendPattern("-dd-MM").toFormatter.withResolverStyle(STRICT)
val parsers = List(DMY, MDY, YMD, YDM).map(createParser)

val nineEleven = LocalDate.parse("2001-09-11")
def wroteOnNineEleven(entries: List[String]) = parsers
  .flatMap(_.apply(entries))
  .contains(nineEleven)

entries.flatMap({
  case (person, entries) if wroteOnNineEleven(entries) => Some(person)
  case _ => None
}).sorted.mkString(" ")