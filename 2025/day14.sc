import common.loadPackets
import fastparse.*
import NoWhitespace.*
import org.apache.commons.numbers.fraction.Fraction

val input = loadPackets(List("day14.txt"))

case object kanji {
  val shakuMeter = Fraction.of(10, 33)
  def digit[$: P]: P[Long] = P(CharIn("一二三四五六七八九").!.map("一二三四五六七八九".indexOf(_) + 1))
  def ten[$: P]: P[Long] = P(CharIn("十百千").!.map(p => Math.pow(10, "十百千".indexOf(p) + 1).toLong))
  def myriad[$: P]: P[Long] = P(CharIn("万億").!.map({
    case "万" => 10000L
    case "億" => 100000000L
  }))
  def smallNumber[$: P]: P[Long] = P(((digit ~ ten.?) | ten).rep(min = 1)
    .map(_.map({
      case (digit, tens) => digit * tens.getOrElse(1L)
      case tens: Long => tens
    }).sum))
  def number[$: P]: P[Long] = P((smallNumber ~ myriad.?).rep(min = 1)
    .map(_.map((number, myriad) => number * myriad.getOrElse(1L)).sum))
  def shaku[$: P]: P[Fraction] = P("尺").map(_ => shakuMeter)
  def ken[$: P]: P[Fraction] = P("間").map(_ => shakuMeter.multiply(6))
  def jo[$: P]: P[Fraction] = P("丈").map(_ => shakuMeter.multiply(10))
  def cho[$: P]: P[Fraction] = P("町").map(_ => shakuMeter.multiply(360))
  def ri[$: P]: P[Fraction] = P("里").map(_ => shakuMeter.multiply(12960))
  def mo[$: P]: P[Fraction] = P("毛").map(_ => shakuMeter.divide(10000))
  def rin[$: P]: P[Fraction] = P("厘").map(_ => shakuMeter.divide(1000))
  def bu[$: P]: P[Fraction] = P("分").map(_ => shakuMeter.divide(100))
  def sun[$: P]: P[Fraction] = P("寸").map(_ => shakuMeter.divide(10))
  def unit[$: P]: P[Fraction] = P(shaku | ken | jo | cho | ri | mo | rin | bu | sun)

  def area[$: P]: P[Long] = P((number ~ unit ~ " × " ~ number ~ unit ~ End)
    .map((n1, u1, n2, u2) => {
      val unit = u1.multiply(u2)
      n1 * n2 * unit.getNumerator.toLong / unit.getDenominator.toLong
    }))
}

input.map(parse(_, kanji.area) match {
  case Parsed.Success(area, _) => area
}).sum