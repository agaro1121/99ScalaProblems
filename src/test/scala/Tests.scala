import org.scalatest.{Matchers, Pending, WordSpec}

class Tests extends WordSpec with Matchers {

  "First 10 Problems should be successfully implemented" should {
    import main.OneToTen._
    val testList = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    "1. Find the last element of the list" in {
      one(testList) shouldBe 10
    }

    "2. Find the last but one element of a list" in {
      two(testList) shouldBe 9
    }

    "3. Find the Kth element of a list" in {
      three(3, testList) shouldBe 4
    }

    "4. Find the number of elements of a list" in {
      four(testList) shouldBe 10
      four2(testList) shouldBe 10
    }

    "5. Reverse a list" in {
      five(testList) shouldBe List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
      testList.reverse shouldBe List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    }

    "6. Find out whether a list is a palindrome" in {
      six(testList) shouldBe false
      six(List(1, 2, 2, 1)) shouldBe true
    }

    "7. Flatten a nested list structure" in {
      seven(List(List(1, 1), 2, List(3, List(5, 8)))) shouldBe List(1, 1, 2, 3, 5, 8)
    }

    "8. Eliminate consecutive duplicates of list elements" in {
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val output = List('a, 'b, 'c, 'a, 'd, 'e)
      eight(input) shouldBe output
      eight2(input) shouldBe output
    }

    "9. Pack consecutive duplicates of list elements into sublists" in {
      Pending
    }

    "10. Run-length encoding of a lis" in {
      Pending
    }

  }

}
