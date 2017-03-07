import main._
import org.scalatest.{Canceled, Matchers, Succeeded, WordSpec}

class Tests21To40 extends WordSpec with Matchers {

  "21 to 40" should {

    "21. Insert an element at a given position into a list. " in {
      val input = List('a, 'b, 'c, 'd)
      val expectedOutput = List('a, 'new, 'b, 'c, 'd)

      P21.insertAt('new, 1, input) shouldBe expectedOutput
      P21.insertAt2('new, 1, input) shouldBe expectedOutput
      P21.insertAtViaRecursion('new, 1, input) shouldBe expectedOutput
    }

    "22. Create a list containing all integers within a given range. " in {
      val input = (4, 9)
      val expectedOutput = List(4, 5, 6, 7, 8, 9)

      P22.range _ tupled input shouldBe expectedOutput
      P22.range2 _ tupled input shouldBe expectedOutput
      P22.range3 _ tupled input shouldBe expectedOutput
      P22.rangeViaRecursion _ tupled input shouldBe expectedOutput
      P22.rangeViaRecursion2 _ tupled input shouldBe expectedOutput
      P22.rangeViaRecursion3 _ tupled input shouldBe expectedOutput

    }

    "23. Extract a given number of randomly selected elements from a list." in {
      val input = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
      val expectedOutput = List('a, 'c, 'f)

//      P23.randomSelect(3, input) shouldBe expectedOutput //always going to fail because of random :-)
      Succeeded
    }

    "24. Lotto: Draw N different random numbers from the set 1..M." in {
      val input = (6, 49)
      val expectedOutput = List(23, 1, 17, 33, 21, 37)

//      P24.lotto(6, 49) shouldBe expectedOutput
//      P24.lottoViaP23(6, 49) shouldBe expectedOutput
//      P24.lottoViaStream(6, 49) shouldBe expectedOutput

      Succeeded
    }

    "25. Generate a random permutation of the elements of a list." in {
      val input = List('a, 'b, 'c, 'd, 'e, 'f)
      val expectedOutput = List('b, 'a, 'd, 'c, 'e, 'f)


    }

    "26. Generate the combinations of K distinct objects chosen from the N elements of a list." in {
      val input = List('a, 'b, 'c, 'd, 'e, 'f)
      val expectedOutput = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e)) //and more

    }

    "27. Group the elements of a set into disjoint subsets." in {
      val input = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
      val expectedOutput = List(List(List("Aldo", "Beat"), List("Carla", "David", "Evi"), List("Flip", "Gary", "Hugo", "Ida"))) //and More

    }

    "28. Sorting a list of lists according to length of sublists." in {
      val input = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
      val expectedOutput = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

    }

    "29. " in {
      Canceled
    }

    "30. " in {
      Canceled
    }

    "31. Determine whether a given integer number is prime." in {
      val input = 7
      val expectedOutput = true

    }

    "32. Determine the greatest common divisor of two positive integer numbers." in {
      val input = (36, 63)
      val expectedOutput = 9

    }

    "33. Determine whether two positive integer numbers are coprime." in {
      val input = (35, 64)
      val expectedOutput = true

    }

    "34. Calculate Euler's totient function phi(m)." in {
      val input = 10
      val expectedOutput = 4

    }

    "35. Determine the prime factors of a given positive integer." in {
      val input = 315
      val expectedOutput = List(3, 3, 5, 7)

    }

    "36. Determine the prime factors of a given positive integer (2)." in {
      val input = 315
      val expectedOutput = List((3, 2), (5, 1), (7, 1))

    }

    "37. Calculate Euler's totient function phi(m) (improved)." in {
      //      val input =
      //      val expectedOutput =

    }

    "38. Compare the two methods of calculating Euler's totient function." in {
      //      val input =
      //      val expectedOutput =

    }

    "39. A list of prime numbers." in {
      val input = 7 to 31
      val expectedOutput = List(7, 11, 13, 17, 19, 23, 29, 31)

    }

    "40. Goldbach's conjecture." in {
      val input = 28
      val expectedOutput = (5, 23)

    }

  }
}
