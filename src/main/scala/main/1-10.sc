/** 1. Find the last element of a list*/
def last[A](list: List[A]): A = {
  require(list != Nil, "Your List is empty !!!")
  list match {
    case x :: Nil => x
    case x :: xs => last(xs)
  }
}
last(List(1, 1, 2, 3, 5, 8))

/** 2. Find the last but one element of a list*/
def penultimate[A](list: List[A]): A = list match {
    case x :: _ :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }
penultimate(List(1, 1, 2, 3, 5, 8))
