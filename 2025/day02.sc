import common.loadPackets

import java.nio.charset.StandardCharsets.UTF_8
import java.time.{OffsetDateTime, ZoneOffset}

val timestamps = loadPackets(List("day02-i18n.txt"))

val waveTime = timestamps.map(OffsetDateTime.parse)
  .map(_.toInstant)
  .groupBy(identity)
  .filter(_._2.size >= 4)
  .keys
  .head.atOffset(ZoneOffset.of("+00:00"))