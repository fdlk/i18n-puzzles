import common.loadPackets

import java.text.Normalizer
import java.text.Normalizer.Form

val passwords = loadPackets(List("day08.txt"))

def letters(value: String) =
  Normalizer.normalize(value, Form.NFKD)
    .filter(_.isLetter)
    .map(_.toLower)

def isVowel(char: Char) = "aeiou".contains(char)
def isConsonant(char: Char) = !isVowel(char)
def noRecurringLetters(string: String) = string.toSet.size == string.length

passwords
  .filter(_.length >= 4)
  .filter(_.length <= 12)
  .filter(_.exists(_.isDigit))
  .map(letters)
  .filter(_.exists(isVowel))
  .filter(_.exists(isConsonant))
  .count(noRecurringLetters)