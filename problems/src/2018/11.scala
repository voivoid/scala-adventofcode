package adventOfCode.problems
package year2018

object problem11 extends baseProblem {

  override def solve1(input: Input): String = {
    val sn = input.mkString.toInt
    val grid = makeCellSummedTable(sn)

    val maxPower = findMaxPowerCoord(grid, squareSide = 3)

    s"${maxPower.x},${maxPower.y}"
  }

  override def solve2(input: Input): String = {
    val sn = input.mkString.toInt
    val grid = makeCellSummedTable(sn)

    val maxPower = (1 to 300).iterator.map(findMaxPowerCoord(grid, _)).maxBy(_.power)

    s"${maxPower.x},${maxPower.y},${maxPower.side}"
  }

  private type SumGrid = Vector[Vector[Int]]

  private def findMaxPowerCoord(grid: SumGrid, squareSide: Int) = {
    val toCoord = GridSide - squareSide + 1

    val squarePowers = for {
      x <- Iterator.from(1).take(toCoord)
      y <- Iterator.from(1).take(toCoord)
    } yield (x, y, calcPower(grid, x, y, squareSide))

    val (x, y, p) = squarePowers.maxBy { case (_, _, power) => power }

    MaxPowerSquare(x, y, p, squareSide)
  }

  private def calcPower(grid: SumGrid, left: Int, top: Int, squareSide: Int): Int = {
    val right = left + squareSide - 1
    val bottom = top + squareSide - 1

    require(right <= GridSide)
    require(bottom <= GridSide)

    val a = grid(top - 1)(left - 1)
    val b = grid(top - 1)(right)
    val c = grid(bottom)(left - 1)
    val d = grid(bottom)(right)

    d + a - b - c
  }

  private case class MaxPowerSquare(x: Int, y: Int, power: Int, side: Int)

  private[problems] def calcFuelCell(x: Int, y: Int, sn: Int): Int = {
    val rackId = x + 10
    val powerLevel = (rackId * y + sn) * rackId
    val hunderdsDigit = (powerLevel / 100) % 10
    hunderdsDigit - 5
  }

  private def GridSide = 300

  // https://en.wikipedia.org/wiki/Summed-area_table
  private def makeCellSummedTable(sn: Int): SumGrid = {

    val cells = for {
      y <- Iterator.from(1).take(GridSide)
      x <- Iterator.from(1).take(GridSide)
    } yield calcFuelCell(x, y, sn)

    val grid = cells.grouped(GridSide).map(_.toVector).toVector

    val horPartialSum = grid.map(_.scanLeft(0)(_ + _))
    val verPartialSum = horPartialSum.transpose.map(_.scanLeft(0)(_ + _))

    val summedTable = verPartialSum.transpose
    summedTable
  }

}
