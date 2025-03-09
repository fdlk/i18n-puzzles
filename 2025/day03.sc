import common.loadPackets

val passwords = loadPackets(List("day03.txt"))

passwords
  .filter(_.length >= 4)
  .filter(_.length <= 12)
  .filter(_.exists(_.isDigit))
  .filter(_.exists(_.isUpper))
  .filter(_.exists(_.isLower))
  .count(_.exists(_.toInt > 127))

