import common.loadPackets

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

val input = loadPackets(List("day04.txt"))
  .map(_.replaceAll("""(Departure|Arrival):\s*""", ""))
  .map(_.replaceAll("""\s{2,}""", " "))

val travels = input
  .grouped(3)
  .map(_.take(2)
    .map(ZonedDateTime.parse(_, DateTimeFormatter.ofPattern("vvvv MMM dd, yyyy, HH:mm")))
    .map(_.toInstant)
  ).map {
    case List(departure, arrival) => departure.until(arrival, ChronoUnit.MINUTES)
  }
  .sum
