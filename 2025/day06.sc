import common.loadPackets

import java.nio.charset.Charset
import scala.annotation.tailrec

val input = loadPackets(List("day06.txt"))

@tailrec
def decode(mojibake: String, times: Int = 1): String =
  if times == 0 then mojibake
  else {
    val bytes = mojibake.getBytes(Charset.forName("iso-8859-1"))
    decode(new String(bytes, Charset.forName("UTF-8")), times - 1)
  }

val words = input.takeWhile(_.nonEmpty).zipWithIndex.map({
  case (word, index) if (index + 1) % 15 == 0 => decode(word, 2)
  case (word, index) if (index + 1) % 5 == 0 => decode(word)
  case (word, index) if (index + 1) % 3 == 0 => decode(word)
  case (word, _) => word
})

val puzzle = input.dropWhile(_.nonEmpty).tail.map(_.trim)

puzzle.map(pattern => words.indexWhere(pattern.r.matches))
  .map(_ + 1)
  .sum