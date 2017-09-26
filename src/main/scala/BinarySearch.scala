package csbasics

import Ordered._
import scala.annotation.tailrec

object BinarySearch {

//  Let min = 0 and max = n-1.
//  If max < min, then stop: target is not present in array. Return -1.
//  Compute guess as the average of max and min, rounded down (so that it is an integer).
//    If array[guess] equals target, then stop. You found it! Return guess.
//  If the guess was too low, that is, array[guess] < target, then set min = guess + 1.
//  Otherwise, the guess was too high. Set max = guess - 1.
//  Go back to step 2.

  def searchAnOrderedList[A : Ordering](orderedList: List[A])
                         (lookingFor: A): Option[A] = {
    val min = 0
    val max = orderedList.length - 1

    @tailrec
    def looper(min: Int, max: Int): Option[A] = {
      if (max < min || max == min) {
        orderedList.lift(min) match {
          case None => Option.empty[A]
          case Some(x) => if (x == lookingFor) Some(x) else Option.empty[A]
        }
      }
      else {
        val guess: Int = (min + max) / 2

        orderedList.lift(guess) match {
          case None => looper(min, guess - 1)
          case Some(x) =>
            if (x == lookingFor) Some(lookingFor)
            else if (x < lookingFor) looper(guess + 1, max)
            else looper(min, guess - 1)
        }
      }
    }

    looper(min, max)
  }
}

object Main extends App {

  val searchIntList = BinarySearch.searchAnOrderedList(List(1,3,5,7,9,11)) _

  println(s"Search for 5: ${searchIntList(5)}")
  println(s"Search for 11: ${searchIntList(11)}")
  println(s"Search for something not there: ${searchIntList(81)}")

  val searchWordList = BinarySearch.searchAnOrderedList(
    List("armadillo", "bat", "cardinal", "dinosaur", "ermine", "fish")) _

  println(s"Search for ermine: ${searchWordList("ermine")}")
  println(s"Search for bat: ${searchWordList("bat")}")
  println(s"Search for something not there: ${searchWordList("unicorn")}")
}