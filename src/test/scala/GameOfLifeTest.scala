import main.{Cell, GameOfLife}
import org.scalatest.{Matchers, WordSpec}

class GameOfLifeTest extends WordSpec with Matchers {

  "Game of Life" should {
    "get neighbors including itself" in {

      val expectedNeighbors =
        Seq(Cell(1,1), Cell(1,2), Cell(1,3), Cell(2,1), Cell(2,2), Cell(2,3), Cell(3,1), Cell(3,2), Cell(3,3))

      val actualNeighbors =
        GameOfLife.getSurroundingCells(Cell(2,2))


      actualNeighbors should contain allElementsOf expectedNeighbors
      actualNeighbors.size shouldBe 9
    }

    "get neighbors except itself" in {

      val expectedNeighbors = Set(Cell(3,1), Cell(1,1), Cell(3,2), Cell(1,3), Cell(3,3), Cell(2,3), Cell(1,2), Cell(2,1))

      val actualNeighbors =
        GameOfLife.getNeighbors(Cell(2,2))


      actualNeighbors should contain allElementsOf expectedNeighbors
      actualNeighbors.size shouldBe 8
    }

    "apply the rules correctly" in {
      val input = Set(
        Cell(2, -2),
        Cell(3, -2),
        Cell(4, -2),
        Cell(3, -1),
        Cell(4, -1)
      )

      val expectedResult = Set(
        Cell(2, -1),
        Cell(4, -1),
        Cell(2, -2),
        Cell(4, -2),
        Cell(3, -3)
      )

      val result = GameOfLife.applyRules(input)

      println(GameOfLife.strigify(input))
      println(GameOfLife.strigify(result))

      result should contain allElementsOf expectedResult
    }

  }

}
