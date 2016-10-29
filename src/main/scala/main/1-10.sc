/** 1. Find the last element of a list */
def last[A](list: List[A]): A = {
  require(list != Nil, "Your List is empty !!!")
  list match {
    case x :: Nil => x
    case x :: xs => last(xs)
  }
}
last(List(1, 1, 2, 3, 5, 8))

/** 2. Find the last but one element of a list */
def penultimate[A](list: List[A]): A = list match {
  case x :: _ :: Nil => x
  case x :: xs => penultimate(xs)
  case _ => throw new NoSuchElementException
}
penultimate(List(1, 1, 2, 3, 5, 8))

/*
* P03 (*) Find the Kth element of a list.
By convention, the first element in the list is element 0.
Example:

scala> nth(2, List(1, 1, 2, 3, 5, 8))
res0: Int = 2
* */

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
nth(2, List(1, 1, 2, 3, 5, 8))

/*
* P04 (*) Find the number of elements of a list.
Example:
scala> length(List(1, 1, 2, 3, 5, 8))
res0: Int = 6
* */
def length[T](list: List[T]): Int = {
  //  list.foldLeft(0)((acc, _) ⇒ acc + 1)
  def loop(acc : Int,list: List[T]): Int = {
    list match {
      case Nil ⇒ 0
      case x :: Nil ⇒ acc + 1
      case x :: xs ⇒ loop(acc +  1, xs)
    }
  }
  loop(0,list)
}
length(List(1, 1, 2, 3, 5, 8))