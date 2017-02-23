package main

import scala.collection.immutable.{Queue, Stack}

object OneToTen {

  def one[A](list: List[A]): A = {
    require(list != Nil, "Your List is empty !!!")
    (list: @unchecked) match {
      case x :: Nil => x
      case _ :: xs => one(xs)
    }
  }

  def two[A](list: List[A]): A = list match {
    case x :: _ :: Nil => x
    case _ :: xs => two(xs)
    case _ => throw new NoSuchElementException
  }

  def three[T](index: Int, list: List[T]): T = {
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

  def four2[T](list: List[T]): Int = list.foldLeft(0)((acc, _) ⇒ acc + 1)
  def four[T](list: List[T]): Int = {
    def loop(acc: Int, list: List[T]): Int = {
      list match {
        case Nil ⇒ 0
        case x :: Nil ⇒ acc + 1
        case x :: xs ⇒ loop(acc + 1, xs)
      }
    }
    loop(0, list)
  }

  def five[T](list: List[T]): List[T] = {
    list.foldLeft(Nil: List[T])((acc, elem) ⇒ elem :: acc)
  }

  def six[T](list: List[T]): Boolean = {
    list.reverse == list
  }

  def seven[T](list: List[T]): List[T] = {
    list.foldLeft(Nil: List[T]) {
      (acc, elem) ⇒
        elem match {
          case l: List[_] ⇒ (acc ++ seven(l)).asInstanceOf[List[T]]
          case e ⇒ acc :+ e
        }
    }
  }

  def eight[T](list: List[T]): List[T] = {
    @scala.annotation.tailrec
    def loop(list: List[T], acc: List[T]): List[T] = {
      list match {
        case Nil ⇒ acc
        case x::xs ⇒ loop(xs,
          if(!acc.headOption.contains(x)) x :: acc
          else acc
        )
      }
    }

    loop(list, Nil).reverse
  }

  def eight2[T](list: List[T]): List[T] = {
    @scala.annotation.tailrec
    def loop(list: List[T], acc: Stack[T]): Stack[T] = {
      list match {
        case Nil ⇒ acc
        case x::xs ⇒ loop(xs,
          if(!acc.headOption.contains(x)) acc.push(x)
          else acc
        )
      }
    }

    loop(list, Stack.empty).toList.reverse
  }

}
