package main

import scala.annotation.tailrec

object P11 {

  def encodeModified[T](list: List[T]): List[Any] = {
    @tailrec
    def loop(list: List[(Int, T)], acc: List[Any]): List[Any] = {
      list match {
        case Nil ⇒ acc.reverse
        case x :: xs ⇒ loop(
          xs,
          {
            val (size, element) = x
            if (size == 1) element :: acc
            else x :: acc
          }
        )
      }
    }
    loop(P10.encode(list), Nil)
  }

  def encodeModifiedViaMap[T](list: List[T]): List[Any] = {
    P10.encode(list).map {
      case t @ (size, element) ⇒
        if (size == 1) element
        else t
    }
  }

  def encodeModifiedViaFoldLeft[T](list: List[T]): List[Any] = {
    P10.encode(list).foldLeft(Nil: List[Any]) {
      case (acc, dupes @ (size, element)) ⇒
        if (size == 1) element :: acc
        else dupes :: acc
    }.reverse
  }

  def encodeModifiedViaFoldRight[T](list: List[T]): List[Any] = {
    P10.encode(list).foldRight(Nil: List[Any]) {
      case (dupes @ (size, element), acc) ⇒
        if (size == 1) element :: acc
        else dupes :: acc
    }
  }

}

object P12 {

  def decode[T](list: List[(Int, T)]): List[T] = {
      list.flatMap {
        case (size, element) ⇒
          List.fill(size)(element)
      }
  }

  def decodeViaFoldLeft[T](list: List[(Int, T)]): List[T] = {
    list.foldLeft(Nil: List[T]){
      case (acc, (size, element)) ⇒
        List.fill(size)(element) ++ acc
    }.reverse
  }

  def decodeViaFoldRight[T](list: List[(Int, T)]): List[T] = {
    list.foldRight(Nil: List[T]){
      case ((size, element), acc) ⇒
        List.fill(size)(element) ++ acc
    }
  }

}

object P13 {

  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    @tailrec
    def loop(list: List[T], acc: List[(Int, T)]): List[(Int, T)] = {
      list match {
        case Nil ⇒ acc.reverse
        case x::_ ⇒
          val (dupes, afterDupes) = list.span(_ == x)
          loop(afterDupes, (dupes.size, x) :: acc)
      }
    }
    loop(list, Nil)
  }

}

object P14 {

  /*
  * Can do any variation of: List(e,e), e :: e
  * and also implement this recursively and with the folds
  * */
  def duplicate[T](list: List[T]): List[T] = {
    list.flatMap{
      elem ⇒
        List.fill(2)(elem)
    }
  }

}

object P15 {

  def duplicateN[T](n: Int, list: List[T]): List[T] = {
    list.flatMap{
      elem ⇒
        List.fill(n)(elem)
    }
  }
}

object P16 {

  def drop[T](n: Int, list: List[T]): List[T] = {
    @tailrec
    def loop(count: Int, list: List[T], acc: List[T]): List[T] = {
       list match {
         case Nil ⇒ acc.reverse
         case x::xs ⇒
           loop(count + 1, xs, if(count > 0 && count % n == 0) acc else x :: acc)
       }

    }
  loop(1, list, Nil)
  }

  def dropWithZip[T](n: Int, list: List[T]): List[T] = {
    val tuples = list.zip(Stream.from(1))
    tuples.collect {
      case (elem, index) if index % n != 0 ⇒ elem
    }
  }

  def dropWithZipWithIndex[T](n: Int, list: List[T]): List[T] = {
    val tuples = list.zipWithIndex
    tuples.filter(t => (t._2 + 1) % n != 0).map(_._1)
  }

}

object P17 {

}

object P18 {

}

object P19 {

}

object P20 {

}
