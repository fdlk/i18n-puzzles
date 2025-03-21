import common.loadPackets

import java.time.DayOfWeek.*
import java.time.ZoneOffset.UTC
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder}
import java.time.{DayOfWeek, Duration, Instant, LocalDate, LocalTime, Year, ZoneId}
import java.util.Locale

val input = loadPackets(List("day15.txt"))
val workdays: Set[DayOfWeek] = Set(MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY)

type Range = com.google.common.collect.Range[Instant]
def range(from: Instant, to: Instant): Range = com.google.common.collect.Range.closed(from, to)

case class Office(name: String, location: ZoneId, holidays: Set[LocalDate], isCustomer: Boolean = false) {
  val startOfShift: LocalTime = LocalTime.parse("08:30")
  val endOfShift: LocalTime = LocalTime.parse("17:00")
  def supportOn(date: LocalDate): Set[Range] = {
    val from = date.atStartOfDay(UTC).withZoneSameInstant(location)
    val to = date.plusDays(1).atStartOfDay(UTC).withZoneSameInstant(location)
    val utcDay = range(from.toInstant, to.toInstant)
    Set(from.toLocalDate, to.toLocalDate)
      .filter(day => workdays.contains(day.getDayOfWeek))
      .diff(holidays)
      .map(day => if isCustomer
      then range(day.atStartOfDay().atZone(location).toInstant, day.plusDays(1).atStartOfDay().atZone(location).toInstant)
      else range(day.atTime(startOfShift).atZone(location).toInstant, day.atTime(endOfShift).atZone(location).toInstant))
      .filter(_.isConnected(utcDay))
      .map(_.intersection(utcDay))
  }
}

def parseOffice(line: String) =
  val dateFormat = new DateTimeFormatterBuilder().appendPattern("d MMMM yyyy").toFormatter(Locale("en"))
  line match {
    case s"${name}\t${zone}\t${holidays}" =>
      Office(name, ZoneId.of(zone), holidays.split(";").map(LocalDate.parse(_, dateFormat)).toSet)
  }

def changes(rng: Range): Set[Instant] = Set(rng.lowerEndpoint(), rng.upperEndpoint())
def minutes(rng: Range): Long = Duration.between(rng.lowerEndpoint(), rng.upperEndpoint()).toMinutes
def isCovered(intervals: Iterable[Range])(interval: Range): Boolean = intervals.exists(_.encloses(interval))

def overtimeRequired(customer: Office, offices: List[Office], day: LocalDate): Long = {
  val available = offices.flatMap(_.supportOn(day))
  val promised = customer.supportOn(day)
  val intervals = Set(available, promised).flatMap(_.flatMap(changes)).toList.sorted
    .sliding(2)
    .filter(_.length == 2)
    .map {
      case List(from, to) => range(from, to)
    }
  val support = isCovered(available)
  val required = isCovered(promised)
  val overtime = intervals.filter(required).filter(!support(_)).toList
  overtime.map(minutes).sum
}

val year = Year.of(2022)
val days = LazyList.iterate(year.atDay(1))(_.plusDays(1)).takeWhile(_.getYear == year.getValue)
val offices = input.takeWhile(_.nonEmpty).map(parseOffice)
val customers = input.dropWhile(_.nonEmpty).tail.map(parseOffice).map(_.copy(isCustomer = true))
val overtimes = customers.map(customer => days.map(overtimeRequired(customer, offices, _)).sum)

overtimes.max - overtimes.min