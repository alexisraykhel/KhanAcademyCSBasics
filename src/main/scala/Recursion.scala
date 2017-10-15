package csbasics


import scala.annotation.tailrec


object Recursion {

  def factorial(number: Int): Int = {
    @tailrec
    def looper(i: Int, acc: Int): Int =
      if (i == 0) acc
      else looper(i-1, acc + i)

    looper(number, 0)
  }

  def amIAPalindrome(word: String): Boolean = {
    @tailrec
    def looper(word: String): Boolean =
      if (word.length == 1 | word.length == 0) true
      else if (word.head == word.last) looper(word.substring(1, word.length - 1))
      else false

    looper(word)
  }
}

object RecursionApp extends App {
  println(Recursion.factorial(5))
  println(Recursion.amIAPalindrome("racecar"))
  println(Recursion.amIAPalindrome("palindrome"))
}
