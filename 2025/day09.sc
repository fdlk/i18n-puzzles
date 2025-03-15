import common.loadPackets

import java.time.{DateTimeException, LocalDate}
import java.time.format.ResolverStyle.STRICT
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.temporal.ChronoField.YEAR

val input = loadPackets(List("day09.txt"))

val entries = input.map(_.split(":").map(_.trim)).flatMap({
  case Array(date, people) => people.split(", ").map(person => person -> date)
}).groupBy(_._1).map({
  case (person, entries) => (person, entries.map(_._2))
}).toVector

def tryParse(dates: List[String], format: DateTimeFormatter) =
  try {
    dates.map(LocalDate.parse(_, format))
  } catch {
    case e: DateTimeException => List()
  }

val DMY = new DateTimeFormatterBuilder().appendPattern("dd-MM-").appendValueReduced(YEAR, 2, 2, 1920).toFormatter.withResolverStyle(STRICT)
val MDY = new DateTimeFormatterBuilder().appendPattern("MM-dd-").appendValueReduced(YEAR, 2, 2, 1920).toFormatter.withResolverStyle(STRICT)
val YMD = new DateTimeFormatterBuilder().appendValueReduced(YEAR, 2, 2, 1920).appendPattern("-MM-dd").toFormatter.withResolverStyle(STRICT)
val YDM = new DateTimeFormatterBuilder().appendValueReduced(YEAR, 2, 2, 1920).appendPattern("-dd-MM").toFormatter.withResolverStyle(STRICT)

def dates(entries: List[String]) = List(DMY, MDY, YMD, YDM).flatMap(format => tryParse(entries, format))

val nineEleven = LocalDate.parse("2001-09-11")
entries.flatMap({
  case (person, entries) if dates(entries).contains(nineEleven) => Some(person)
  case _ => None
}).sorted.mkString(" ")