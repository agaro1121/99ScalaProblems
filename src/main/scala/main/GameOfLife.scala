package main

final case class Cell(x: Int, y: Int)

object GameOfLife extends App {

  def getSurroundingCells(cell: Cell): Set[Cell] =
    (for {
      dx <- -1 to 1
      dy <- -1 to 1
    } yield Cell(cell.x + dx, cell.y + dy)).toSet

  def getNeighbors(cell: Cell): Set[Cell] =
    getSurroundingCells(cell) - cell

  def applyRules(cells: Set[Cell]): Set[Cell] = {
    cells.flatMap{ cell =>
      getSurroundingCells(cell).filter{ cell =>
        val numOfNeighbors = getNeighbors(cell).count(cells)
        numOfNeighbors == 3 || numOfNeighbors == 2 && cells(cell)
      }
    }
  }

  def strigify(cells: Set[Cell]): String = {
    val size = 5
    val border = "/" * size * 2
    s"""
       |$border
       |${
          List.tabulate(size, size){
            (row, column) => if(cells(Cell(column,-row))) "1 " else "0 "
          }.map(row => row.mkString).mkString("\n")
         }
       |$border
     """.stripMargin

  }

}

