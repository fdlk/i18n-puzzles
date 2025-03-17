import common.loadPackets

val input = loadPackets(List("day11.txt"))

val lowercase = "ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ"
val uppercase = "αβγδεζηθικλμνξοπρστυφχψω"
val variants = Set("Οδυσσευς", "Οδυσσεως", "Οδυσσει", "Οδυσσεα", "Οδυσσευ")

def rot(n: Int)(line: String): String =
  line
    .replaceAll("ς", "σ")
    .map(c => (c, lowercase.indexOf(c), uppercase.indexOf(c)))
    .map({
      case (c, -1, -1) => c
      case (c, l, _) if l >= 0 => lowercase((l + n) % lowercase.length)
      case (c, _, u) if u >= 0 => uppercase((u + n) % uppercase.length)
    })
    .mkString

val rots = lowercase.indices.map(rot)

def findRot(line: String): Int =
  rots.map(_(line))
    .indexWhere(rotatedLine => variants.exists(rotatedLine.contains))

input.map(findRot).filter(_ >= 0).sum