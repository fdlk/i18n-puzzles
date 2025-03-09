import common.loadPackets

import java.nio.charset.StandardCharsets.UTF_8
import java.time.ZoneOffset.UTC
import java.time.chrono.IsoChronology
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, ResolverStyle}
import java.time.{OffsetDateTime, ZoneOffset}

val timestamps = loadPackets(List("day02-i18n.txt"))
val formatter = new DateTimeFormatterBuilder()
  .append(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  .appendOffset("+HH:MM", "+00:00")
  .toFormatter

val waveTime = timestamps.map(OffsetDateTime.parse)
  .map(_.toInstant)
  .groupBy(identity)
  .filter(_._2.size >= 4)
  .keys
  .head
  .atOffset(UTC)
  .format(formatter)