import common.{Memo, loadPackets}
import org.springframework.security.crypto.bcrypt.BCrypt

import java.text.Normalizer.{Form, normalize}
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.*

val input = loadPackets(List("day10.txt"))

type User = String
type Password = String
type Hash = String

val hashedPasswords: Map[User, Hash] = input.takeWhile(!_.isBlank).map(_.split("""\s+""")).map({case Array(a, b) => a -> b}).toMap
val attempts: List[String] = input.drop(hashedPasswords.size + 1)

def optionsForLetter(letter: Char): List[String] =
  List(Form.NFD, Form.NFC).map(normalize(letter.toString, _)).distinct

def optionsForString(password: Password): List[Password] = password.toList match {
  case Nil => List("")
  case letter :: rest =>
    for (prefix <- optionsForLetter(letter);
         postfix <- optionsForString(rest.mkString))
    yield prefix + postfix
}

val bcrypt: Memo[(String, String), Boolean] = Memo(BCrypt.checkpw.tupled)

def matches(user: User, password: Password): Boolean =
  val normalized = normalize(password, Form.NFC)
  optionsForString(normalized).exists(bcrypt(_, hashedPasswords(user)))

def isValidAttempt(attempt: String) =
  val List(user, password) = attempt.split("""\s+""").toList
  matches(user, password)

attempts.par.count(isValidAttempt)