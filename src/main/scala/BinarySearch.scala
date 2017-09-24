package csbasics


object BinarySearch {

//  Let min = 0 and max = n-1.
//  If max < min, then stop: target is not present in array. Return -1.
//  Compute guess as the average of max and min, rounded down (so that it is an integer).
//    If array[guess] equals target, then stop. You found it! Return guess.
//  If the guess was too low, that is, array[guess] < target, then set min = guess + 1.
//  Otherwise, the guess was too high. Set max = guess - 1.
//  Go back to step 2.

  def searchAnOrderedList(orderedList: List[Int])
                         (lookingFor: Int): Option[Int] = {
    val min = 0
    val max = orderedList.length - 1

    val maybeGuess = orderedList.find(_ == lookingFor)
    val maybeGuessIndex = maybeGuess.map(orderedList.indexOf(_))

    def looper(min: Int, max: Int): Option[Int] = {
      if (max < min || max == min) Option.empty[Int]
      else {
        val guess: Int = (min + max) / 2
        val guessedSpot = orderedList(guess)

        if (guessedSpot == lookingFor) Some(lookingFor)
        else {
          if (guessedSpot < lookingFor) looper(guess + 1, max)
          else looper(min, guess - 1)
        }
      }
    }

    looper(min, max)
  }

}

object Main extends App {

  val searchPrep = BinarySearch.searchAnOrderedList(List(1,3,5,7,9,11)) _

  println(s"Search for 5: ${searchPrep(5)}")
  println(s"Search for 11: ${searchPrep(11)}")
  println(s"Search for something not there: ${searchPrep(81)}")
}