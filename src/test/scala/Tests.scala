import main._
import org.scalatest.{Matchers, Pending, WordSpec}

class Tests extends WordSpec with Matchers {

  "First 10 Problems should be successfully implemented" should {
    val testList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    "1. Find the last element of the list" in {
      P01.last(testList) shouldBe 10
    }

    "2. Find the last but one element of a list" in {
      P02.penultimate(testList) shouldBe 9
    }

    "3. Find the Kth element of a list" in {
      P03.nth(3, testList) shouldBe 4
    }

    "4. Find the number of elements of a list" in {
      P04.length(testList) shouldBe 10
      P04.lengthWithFoldLeft(testList) shouldBe 10
    }

    "5. Reverse a list" in {
      P05.reverse(testList) shouldBe List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
      testList.reverse shouldBe List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    }

    "6. Find out whether a list is a palindrome" in {
      P06.isPalindrome(testList) shouldBe false
      P06.isPalindrome(List(1, 2, 2, 1)) shouldBe true
    }

    "7. Flatten a nested list structure" in {
      P07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
    }

    "8. Eliminate consecutive duplicates of list elements" in {
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val output = List('a, 'b, 'c, 'a, 'd, 'e)
      P08.compress(input) shouldBe output
    }

    "9. Pack consecutive duplicates of list elements into sublists" in {
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expectedOutput = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

      P09.pack(input) shouldBe expectedOutput
    }

    "10. Run-length encoding of a list" in {
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expectedOutput: List[(Int, Symbol)] = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))

      P10.encodeWithFoldLeft(input) shouldBe expectedOutput
      P10.encodeWithFoldRight(input) shouldBe expectedOutput
      P10.encode(input) shouldBe expectedOutput
      P10.encodeWithMap(input) shouldBe expectedOutput
    }

    "11. if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms" in {
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expectedOutput = List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e))

      P11.encodeModified(input) shouldBe expectedOutput
      P11.encodeModifiedViaMap(input) shouldBe expectedOutput
      P11.encodeModifiedViaFoldLeft(input) shouldBe expectedOutput
      P11.encodeModifiedViaFoldRight(input) shouldBe expectedOutput

    }

    "12. Given a run-length code list generated as specified in problem P10, construct its uncompressed version" in {
      val input = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
      val expectedOutput = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

      P12.decode(input) shouldBe expectedOutput
      P12.decodeViaFoldLeft(input) shouldBe expectedOutput
      P12.decodeViaFoldRight(input) shouldBe expectedOutput
    }

    "13. Implements the so-called run-length encoding data compression method directly" in {
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expectedOutput = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))

      P13.encodeDirect(input) shouldBe expectedOutput
    }

    "14. Duplicate the elements of a list" in {
      val input = List('a, 'b, 'c, 'c, 'd)
      val expectedOutput = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
      P14.duplicate(input) shouldBe expectedOutput
    }

    "15. Duplicate the elements of a list a given number of times" in {
      val input = List('a, 'b, 'c, 'c, 'd)
      val expectedOutput = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
      P15.duplicateN(3, input) shouldBe expectedOutput
    }

    "16. Drop every Nth element from a list" in {
      val input = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val expectedOut = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

      P16.drop(3, input) shouldBe expectedOut
      P16.dropWithZip(3, input) shouldBe expectedOut
      P16.dropWithZipWithIndex(3, input) shouldBe expectedOut
    }

    "17. Split a list into two parts" in {
      val input = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val expectedOutput = (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

      P17.split(3, input) shouldBe expectedOutput
      P17.splitViaBuiltIn(3, input) shouldBe expectedOutput
      P17.splitViaBuiltIn2(3, input) shouldBe expectedOutput
    }

    "18. Extract a slice from a list" in {
      val input = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val expectedOutput = List('d, 'e, 'f, 'g)

      P18.slice(3, 7, input) shouldBe expectedOutput
      P18.sliceViaBuiltIn(3, 7, input) shouldBe expectedOutput
      P18.sliceViaBuiltIn2(3, 7, input) shouldBe expectedOutput
    }

    "19. Rotate a list N places to the left" in {
      val input = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
      val expectedOutputWithPositive = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
      val expectedOutputWithNegative = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)

      P19.rotate(-2, input) shouldBe expectedOutputWithNegative
      P19.rotate(3, input) shouldBe expectedOutputWithPositive
      P19.rotate2(-2, input) shouldBe expectedOutputWithNegative
      P19.rotate2(3, input) shouldBe expectedOutputWithPositive
      P19.rotateViaRecursion(3, input) shouldBe expectedOutputWithPositive
      P19.rotateViaRecursion(-2, input) shouldBe expectedOutputWithNegative
    }

    "20. Remove the Kth element from a list" in {
      val input = List('a, 'b, 'c, 'd)
      val expectedOutput = (List('a, 'c, 'd), 'b)
      P20.removeAt(1, input) shouldBe expectedOutput
      P20.removeAtViaRecursion(1, input) shouldBe expectedOutput
    }

  }

}
