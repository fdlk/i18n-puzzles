import common.loadPackets

import java.nio.charset.StandardCharsets.UTF_8

val messages = loadPackets(List("day01-i18n.txt"))

def canTweet(msg: String): Boolean = msg.length <= 140
def canSms(msg: String): Boolean = msg.getBytes(UTF_8).length <= 160

def cost(msg: String): Int = (canTweet(msg), canSms(msg)) match
  case (true, true) => 13
  case (true, false) => 7
  case (false, true) => 11
  case (false, false) => 0

messages.map(cost).sum
