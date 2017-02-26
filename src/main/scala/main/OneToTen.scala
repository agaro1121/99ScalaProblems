package main

import scala.annotation.tailrec

object P01 {
  def last[A](list: List[A]): A = {
    require(list != Nil, "Your List is empty !!!")
    (list: @unchecked) match {
      case x :: Nil => x
      case _ :: xs => last(xs)
    }
  }
}

object P02 {
  def penultimate[A](list: List[A]): A = list match {
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }
}

object P03 {
  def nth[T](index: Int, list: List[T]): T = {
    require(index >= 0)

    def loop(count: Int, list: List[T]): T = {
      list match {
        case Nil ⇒ throw new NoSuchElementException
        case x :: xs ⇒
          if (count == index) x
          else loop(count + 1, xs)
      }
    }

    loop(0, list)
  }
}

object P04 {
  def lengthWithFoldLeft[T](list: List[T]): Int = list.foldLeft(0)((acc, _) ⇒ acc + 1)
  def length[T](list: List[T]): Int = {
    def loop(acc: Int, list: List[T]): Int = {
      list match {
        case Nil ⇒ 0
        case x :: Nil ⇒ acc + 1
        case x :: xs ⇒ loop(acc + 1, xs)
      }
    }
    loop(0, list)
  }

}

object P05 {
  def reverse[T](list: List[T]): List[T] = {
    list.foldLeft(Nil: List[T])((acc, elem) ⇒ elem :: acc)
  }
}

object P06 {
  def isPalindrome[T](list: List[T]): Boolean = {
    list.reverse == list
  }
}

object P07 {
  def flatten[T](list: List[T]): List[T] = {
    list.foldLeft(Nil: List[T]) {
      (acc, elem) ⇒
        elem match {
          case l: List[_] ⇒ (acc ++ flatten(l)).asInstanceOf[List[T]]
          case e ⇒ acc :+ e
        }
    }
  }
}

object P08 {

  def compress[T](list: List[T]): List[T] = {
    @scala.annotation.tailrec
    def loop(list: List[T], acc: List[T]): List[T] = {
      list match {
        case Nil ⇒ acc
        case x :: xs ⇒ loop(
          xs,
          if (!acc.headOption.contains(x)) x :: acc
          else acc
        )
      }
    }
    loop(list, Nil).reverse
  }

}

object P09 {

  def pack[T](list: List[T]): List[List[T]] = {
    @tailrec
    def loop(list: List[T], acc: List[List[T]]): List[List[T]] = {
      list match {
        case Nil ⇒ acc.reverse
        case x :: xs ⇒
          val (dupes, restAfterTakingDupes) = xs.span(_ == x)
          loop(restAfterTakingDupes, (x :: dupes) :: acc)
      }
    }
    loop(list, Nil)
  }

}

object P10 {

  def encodeWithFoldLeft[T](list: List[T]): List[(Int, T)] = {
    P09.pack(list).foldLeft(Nil: List[(Int, T)])((acc, elem) ⇒ (elem.size, elem.head) :: acc).reverse
  }

  def encodeWithFoldRight[T](list: List[T]): List[(Int, T)] = {
    P09.pack(list).foldRight(Nil: List[(Int, T)])((elem, acc) ⇒ (elem.size, elem.head) :: acc)
  }

  def encode[T](list: List[T]): List[(Int, T)] = {
    @tailrec
    def loop(list: List[List[T]], acc: List[(Int, T)]): List[(Int, T)] = {
      list match {
        case Nil ⇒ acc.reverse
        case x :: xs ⇒ loop(xs, (x.size, x.head) :: acc)
      }
    }
    loop(P09.pack(list), Nil)
  }

  def encodeWithMap[T](list: List[T]): List[(Int, T)] = {
    P09.pack(list).map(l ⇒ (l.size, l.head))
  }

}