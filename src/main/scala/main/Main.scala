package game.of.life

import scala.language.implicitConversions
import Coordinate.tuple2Coordinate
import scala.util.{Success, Try}

case class Cell(isAlive: Boolean) {
  override def toString: String =
    if (isAlive) "*" else "-"
}

/*******************************************************************************************************************/
case class Coordinate(x: Int, y: Int) {
  override def toString: String = s"($x, $y)"
}
case object Coordinate {
  implicit def tuple2Coordinate(coordinate: (Int, Int)): Coordinate = Coordinate(coordinate._1, coordinate._2)
}

/*******************************************************************************************************************/
case class Space(cell: Option[Cell]) {
  override def toString: String =
    cell.map(c ⇒ s"$c  ").getOrElse(".  ")
}

/*******************************************************************************************************************/
case class Grid(size: Int) {
  private val underlying: Array[Array[Space]] =
    Array.tabulate(size, size)((x, y) ⇒ Space(None))

  def getSpaceAt(coordinate: Coordinate): Space =
    underlying(coordinate.x)(coordinate.y)

  def killCellAt(coordinate: Coordinate): Unit =
    underlying(coordinate.x)(coordinate.y) = Space(None)

  def resurrectCellAt(coordinate: Coordinate): Unit =
    underlying(coordinate.x)(coordinate.y) = Space(Some(Cell(true)))

  def spawnCellAt(coordinate: Coordinate): Unit =
    underlying(coordinate.x)(coordinate.y) = Space(Some(Cell(true)))

  override def toString: String = {
    underlying.map(row ⇒ row.mkString).mkString("\n")
  }
}

object Grid {

  private def findNeighbors(grid: Grid): Map[Coordinate, List[Space]] = {
    (for {
      x ← 0 until grid.size
      y ← 0 until grid.size
    } yield {
      val neighbors = (findNeighborsInSameRow(grid, (x, y)) ++
        findNeighborsInSameColumn(grid, (x, y)) ++
        findNeighborsInDiags(grid, (x, y)))
        .collect {
          case Success(space @ Space(Some(_))) ⇒ space
        }
      Coordinate(x, y) → neighbors
    }).toMap
  }

  private def findNeighborsInSameRow(grid: Grid, coordinate: Coordinate): List[Try[Space]] = {
    import coordinate._
    List(
      Try(grid.getSpaceAt(x + 1, y)),
      Try(grid.getSpaceAt(x - 1, y))
    )
  }
  private def findNeighborsInSameColumn(grid: Grid, coordinate: Coordinate): List[Try[Space]] = {
    import coordinate._
    List(
      Try(grid.getSpaceAt(x, y + 1)),
      Try(grid.getSpaceAt(x, y - 1))
    )
  }
  private def findNeighborsInDiags(grid: Grid, coordinate: Coordinate): List[Try[Space]] = {
    import coordinate._
    List(
      Try(grid.getSpaceAt(x - 1, y + 1)),
      Try(grid.getSpaceAt(x + 1, y - 1)),
      Try(grid.getSpaceAt(x + 1, y + 1)),
      Try(grid.getSpaceAt(x - 1, y - 1))
    )
  }

  private def applyRules(grid: Grid, instructions: Map[Coordinate, List[Space]]): Grid = {
    instructions.foreach {
      case (coordinate, neighbors) ⇒
        grid.getSpaceAt(coordinate) match {
          case Space(None) ⇒
            neighbors.size match {
              case 3 ⇒ grid.spawnCellAt(coordinate)
              case _ ⇒ //nothing
            }
          case _ ⇒
            neighbors.size match {
              case 0 | 1 ⇒ grid.killCellAt(coordinate)
              case 2 | 3 ⇒ //survive - so nothing
              case n if n >= 4 ⇒ grid.killCellAt(coordinate)
            }
        }
    }
    grid
  }

  def nextStep(grid: Grid): Grid = {
    applyRules(grid, findNeighbors(grid))
  }

}

/*******************************************************************************************************************/
object Game extends App {
  import Grid._

  val n = 6
  val grid: Grid = Grid(n)

  val initialConfig =
    grid.spawnCellAt(Coordinate(1, 2))
  grid.spawnCellAt(Coordinate(2, 2))
  grid.spawnCellAt(Coordinate(3, 2))
  grid.spawnCellAt(Coordinate(4, 2))

  println(grid)
  println("//////////////////// Step 1 ///////////////////////////////")
  println(nextStep(grid))
  println("//////////////////// Step 2 ///////////////////////////////")
  println(nextStep(grid))
  println("//////////////////// Step 3 ///////////////////////////////")
  println(nextStep(grid))
  println("//////////////////// Step 4 ///////////////////////////////")
  println(nextStep(grid))
  println("//////////////////// Step 5 ///////////////////////////////")
  println(nextStep(grid))

}