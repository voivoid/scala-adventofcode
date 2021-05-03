package adventOfCode.problems
package year2017

object problem14 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.neighbours4

  override def solve1(input: Input): Int = {
    val grid = makeGrid(input)
    grid.size
  }

  override def solve2(input: Input): Int = {
    val grid = makeGrid(input)
    calcRegions(grid, 0)
  }

  private type Coord = Point[Int]
  private type Grid = Set[Coord]

  private def makeGrid(input: Input): Grid = {
    val inputStr = input.mkString

    val rowStrs = (0 until Side).map(n => inputStr + "-" + n.toString)
    val rowHashes = rowStrs.map(row => problem10.solve2(scala.io.Source.fromString(row)))

    val filledSquares = for {
      (hash, y) <- rowHashes.iterator.zipWithIndex
      ('1', x) <- hash.iterator
        .flatMap(hexDigit => intToBinaryStr(Integer.parseInt(hexDigit.toString, 16)))
        .zipWithIndex
    } yield Point(x, y)

    filledSquares.toSet
  }

  @scala.annotation.tailrec
  private def calcRegions(grid: Grid, regions: Int): Int = {
    if (grid.isEmpty) regions
    else {
      calcRegions(eraseRegion(grid, Set(grid.head)), regions + 1)
    }
  }

  @scala.annotation.tailrec
  private def eraseRegion(grid: Grid, coords: Set[Coord]): Grid = {
    if (coords.isEmpty) grid
    else {
      val c = coords.head
      val neighbours = neighbours4(c).filter(grid.contains)
      eraseRegion(grid - c, (coords - c) ++ neighbours)
    }
  }

  private def intToBinaryStr(int: Int): String = {
    lazy val map = getBinMap
    map(int)
  }

  private def getBinMap: Map[Int, String] = {
    (0 until 16).map(i => i -> String.format("%4s", i.toBinaryString).replace(' ', '0')).toMap
  }

  private def Side = 128

}
