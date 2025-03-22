import common.loadPackets

import java.time.DayOfWeek.*
import java.time.ZoneOffset.UTC
import java.time.format.*
import java.time.*
import java.util.Locale.ENGLISH

val input = loadPackets(List("day15.txt"))

type Range = com.google.common.collect.Range[Instant]
def range(from: Instant, to: Instant): Range = com.google.common.collect.Range.closed(from, to)
def allDay(day: LocalDate, zoneId: ZoneId): Range =
  range(day.atStartOfDay(zoneId).toInstant, day.plusDays(1).atStartOfDay(zoneId).toInstant)
def shift(day: LocalDate, zoneId: ZoneId): Range =
  range(day.atTime(LocalTime.parse("08:30")).atZone(zoneId).toInstant,
    day.atTime(LocalTime.parse("17:00")).atZone(zoneId).toInstant)

extension(r: Range)
  def edges: Set[Instant] = Set(r.lowerEndpoint(), r.upperEndpoint())
  def duration: Duration = Duration.between(r.lowerEndpoint(), r.upperEndpoint())
  def isCoveredBy(intervals: Iterable[Range]): Boolean = intervals.exists(_.encloses(r))

case class Office(name: String, location: ZoneId, holidays: Set[LocalDate], isCustomer: Boolean = false) {
  def supportOn(date: LocalDate): Set[Range] =
    val utcDay = allDay(date, UTC)
    val days = utcDay.edges.map(_.atZone(location).toLocalDate)
    days.filterNot(day => Set(SATURDAY, SUNDAY).contains(day.getDayOfWeek))
      .diff(holidays)
      .map(day => if isCustomer then allDay(day, location) else shift(day, location))
      .filter(utcDay.isConnected)
      .map(utcDay.intersection)
}

def parseOffice(line: String) =
  val dateFormat = new DateTimeFormatterBuilder().appendPattern("d MMMM yyyy").toFormatter(ENGLISH)
  line match {
    case s"${name}\t${zone}\t${holidays}" =>
      Office(name, ZoneId.of(zone), holidays.split(";").map(LocalDate.parse(_, dateFormat)).toSet)
  }
val offices = input.takeWhile(_.nonEmpty).map(parseOffice)
val customers = input.dropWhile(_.nonEmpty).tail.map(parseOffice).map(_.copy(isCustomer = true))

def overtimeRequired(customer: Office, offices: List[Office], day: LocalDate): Long =
  val supported = offices.flatMap(_.supportOn(day))
  val required = customer.supportOn(day)
  val intervals = Set(supported, required)
    .flatMap(_.flatMap(edges)).toList.sorted
    .sliding(2)
    .filter(_.length == 2)
    .map { case List(from, to) => range(from, to) }
  intervals
    .filter(_.isCoveredBy(required))
    .filterNot(_.isCoveredBy(supported))
    .map(_.duration.toMinutes)
    .sum

val year = Year.of(2022)
val days = (1 to year.length).map(year.atDay)
val overtimes = customers.map(customer => days.map(overtimeRequired(customer, offices, _)).sum)
overtimes.max - overtimes.min