package csbasics


import scala.annotation.tailrec

/*
Find the smallest card. Swap it with the first card.
Find the second-smallest card. Swap it with the second card.
Find the third-smallest card. Swap it with the third card.
Repeat finding the next-smallest card, and swapping it into the correct position until the array is sorted.
 */

object SelectionSort {

  def sort[A : Ordering](as: List[A]): List[A] = {

    @tailrec
    def loopDeLoop(withIndices: List[(A, Int)], continuationFrom: Int): List[A] = {
      if (continuationFrom == withIndices.length) withIndices.map(_._1)
      else {
        val subs = withIndices.slice(continuationFrom, withIndices.length)
        loopDeLoop(swap(withIndices, continuationFrom, minimumIndex(subs) + continuationFrom), continuationFrom + 1)
      }
    }

    loopDeLoop(as.zipWithIndex, 0)
  }

  private def minimumIndex[A : Ordering](as: List[(A, Int)]): Int =
    as.indexWhere(_._1 == as.map(_._1).min)

  private def swap[B](as: List[B],
                      firstI: Int,
                      secondI: Int): List[B] = {
    for {
      i <- as.lift(firstI)
      ii <- as.lift(secondI)
    } yield as.updated(firstI, ii).updated(secondI, i)
  }.fold(as)(identity)

}

object SSMain extends App {

  val integerList = List(1,6,28,89,2,0,33,3)
  val alphabetList = List("a","q","w","d","c","b","y","g")

  println(s"Sort Int: ${SelectionSort.sort(integerList)}")
  println(s"Sort Letters: ${SelectionSort.sort(alphabetList)}")
}