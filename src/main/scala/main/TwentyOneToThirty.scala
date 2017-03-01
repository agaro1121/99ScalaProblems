package main

import scala.annotation.tailrec

object P21 {

  def insertAt[T](newElement: T, n: Int, list: List[T]): List[T] = {
    val (left, right) = list.splitAt(n)
    left ::: newElement :: right
  }

  def insertAt2[T](newElement: T, n: Int, list: List[T]): List[T] = {
    list.splitAt(n) match {
      case (left, right) ⇒ left ::: newElement :: right
    }
  }

  def insertAtViaRecursion[T](newElement: T, n: Int, list: List[T]): List[T] = {
    @tailrec
    def loop(count: Int, list: List[T], acc: List[T]): List[T] = {
      (count, list) match {
        case (n, _) if n == 0 ⇒ acc ::: newElement :: list
        case (n, x :: xs) ⇒ loop(n - 1, xs, x :: acc)
      }
    }
    loop(n, list, Nil)
  }
}

object P22 {

  def range(a: Int, b: Int): List[Int] = (a to b).toList
  def range2(a: Int, b: Int): List[Int] = Stream.from(a).take(b - a + 1).toList
  def range3(a: Int, b: Int): List[Int] = List.range(a, b + 1)

  def rangeViaRecursion(a: Int, b: Int): List[Int] = {
    @tailrec
    def loop(count: Int, acc: List[Int]): List[Int] = {
      if(count > b) acc.reverse
      else loop(count + 1, count :: acc)
    }

    loop(a, Nil)
  }

  def rangeViaRecursion3(a: Int, b: Int): List[Int] = {
    @tailrec
    def loop(count: Int, acc: List[Int]): List[Int] = {
      if(count < a) acc
      else loop(count - 1, count :: acc)
    }

    loop(b, Nil)
  }

  def rangeViaRecursion2(a: Int, b: Int): List[Int] = {
      if(b < a) Nil
      else a :: rangeViaRecursion2(a + 1 ,b)
    }


}

object P23 {

}

object P24 {

}

object P25 {

}

object P26 {

}

object P27 {

}

object P28 {

}

object P29 {

}

object P30 {

}
