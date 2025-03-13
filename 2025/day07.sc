import common.loadPackets

import java.time.{OffsetDateTime, ZoneId}

val input = loadPackets(List("day07.txt"))

val halifax = ZoneId.of("America/Halifax")
val santiago = ZoneId.of("America/Santiago")

case class Record(ts: OffsetDateTime, correct: Int, incorrect: Int) {
  val canBeHalifax = ts.atZoneSameInstant(halifax).toOffsetDateTime.getOffset.equals(ts.getOffset)
  val timezone = if canBeHalifax then halifax else santiago
  val correctTs = ts.minusMinutes(incorrect).plusMinutes(correct).atZoneSameInstant(timezone).toOffsetDateTime
}

input.map(_.split("""\s+"""))
  .map {
    case Array(ts, correct, incorrect) => Record(OffsetDateTime.parse(ts), Integer.valueOf(correct), Integer.valueOf(incorrect))
  }.map(_.correctTs)
  .map(_.getHour)
  .zipWithIndex
  .map((hour, index) => hour * (index + 1))
  .sum