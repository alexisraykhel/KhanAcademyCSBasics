package csbasics


import Ordered._


object InsertionSort {

  def insertSort[A : Ordering](list: List[A]): List[A] =
    list.zipWithIndex.foldLeft(List.empty[A])((acc, a) =>
      acc match {
        case Nil => List(a._1)
        case l@_ => insert(l, a._2,   a._1)
      })

  private def insert[A : Ordering](list: List[A], rightIndex: Int, value: A): List[A] = {
    def looper(as: List[(A, Int)]): Int = as match {
      case Nil => 0
      case h :: Nil => if (value >= h._1) h._2 + 1 else h._2
      case h :: t => if (value >= h._1) h._2 + 1 else looper(t)
    }

    val (first, second) = list.splitAt(looper(list.slice(0, rightIndex).zipWithIndex.reverse))
    first ::: value :: second
  }

}

object ISMain extends App {
  val a = List("a", "b", "c", "d", "e" , "b")
  val empty = List.empty[String]
  val ka = List(3, 5, 7, 11, 13, 2, 9, 6)
  InsertionSort.insertSort(a)
}