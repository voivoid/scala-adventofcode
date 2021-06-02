package adventOfCode.problems
package year2019

object problem24 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.neighbours4

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.IteratorFindFirstDuplicate

    val grid = makeGrid(input)

    val firstDupBiodiversity = Iterator.iterate(grid)(calcNextGridState).map(calcBiodiversity).findFirstDuplicateElem.get
    firstDupBiodiversity
  }

  override def solve2(input: Input): Int = {
    solve2(input, minutesToPass = 200)
  }

  def solve2(input: Input, minutesToPass: Int): Int = {
    import adventOfCode.utils.algorithms.IteratorLast

    val multiGrid = Map(0 -> makeGrid(input))
    val finalGrid = Iterator.iterate(multiGrid, minutesToPass + 1)(calcNextMultigrid).last

    val bugsTotal = finalGrid.values.map(countBugs).sum
    bugsTotal
  }

  private type Grid = Array[Array[Char]]
  private type Coord = Point[Int]
  private type Tile = Char
  private type Depth = Int
  private type MultiGrid = Map[Depth, Grid]

  private def makeGrid(input: Input): Grid = input.getLines().toArray.map(_.toArray)

  private def countBugs(grid: Grid): Int = {
    grid.iterator.flatten.count(_ == TileBug)
  }

  private def calcNextGridState(grid: Grid): Grid = {
    Array.tabulate(GridSide, GridSide) {
      case (y, x) => {
        val coord = Point(x, y)
        val neighbours = neighbours4(coord)
        val neighbourBugs = neighbours.map(getGridTile(grid, _)).count(_ == TileBug)
        calcNextTileState(getGridTile(grid, coord), neighbourBugs)
      }
    }
  }

  private def getMultiGridNeighbours(depth: Depth, coord: Coord): List[(Depth, Coord)] = {
    val isOuterLeft = coord.x == 0
    val isOuterRight = coord.x == GridSide - 1
    val isOuterTop = coord.y == 0
    val isOuterBottom = coord.y == GridSide - 1
    val isOuter = isOuterLeft || isOuterRight || isOuterTop || isOuterBottom

    val sameDepthNeighbours = neighbours4(coord).map((depth, _))
    val otherNeighbours =
      if (isOuter) {
        val nextDepth = depth - 1
        val possibleNeighbours = List((isOuterLeft, Point(1, 2)), (isOuterRight, Point(3, 2)), (isOuterTop, Point(2, 1)), (isOuterBottom, Point(2, 3)))

        possibleNeighbours.collect { case (clause, coord) if clause => (nextDepth, coord) }
      } else {
        val nextDepth = depth + 1
        val line = 0 until GridSide
        val tileIndex = coord.y * GridSide + coord.x + 1

        val neighbours = tileIndex match {
          case 8  => line.map(i => (nextDepth, Point(i, 0)))
          case 12 => line.map(i => (nextDepth, Point(0, i)))
          case 14 => line.map(i => (nextDepth, Point(GridSide - 1, i)))
          case 18 => line.map(i => (nextDepth, Point(i, GridSide - 1)))
          case _  => List.empty
        }

        neighbours
      }

    sameDepthNeighbours ++ otherNeighbours
  }

  private def calcNextGridState(multiGrid: MultiGrid, grid: Grid, depth: Depth): Grid = {
    Array.tabulate(GridSide, GridSide) {
      case (GridCenter, GridCenter) => TileEmpty
      case (y, x) => {
        val coord = Point(x, y)
        val neighbours = getMultiGridNeighbours(depth, coord)
        val neighbourBugs = neighbours.count { case (nDepth, nCoord) => getMultiGridTile(multiGrid, nDepth, nCoord) == TileBug }
        calcNextTileState(getGridTile(grid, coord), neighbourBugs)
      }
    }
  }

  private def calcNextMultigrid(multigrid: MultiGrid): MultiGrid = {
    import adventOfCode.utils.algorithms.IterableMinMax

    val resultMG0 = multigrid.map { case (depth, grid) => depth -> calcNextGridState(multigrid, grid, depth) }

    val (outerDepth, innerDepth) = {
      val (min, max) = resultMG0.keys.minmax
      (min - 1, max + 1)
    }

    val outerGrid = calcNextGridState(multigrid, EmptyGrid, outerDepth)
    val innerGrid = calcNextGridState(multigrid, EmptyGrid, innerDepth)

    val resultMG1 = if (countBugs(outerGrid) == 0) resultMG0 else resultMG0 + (outerDepth -> outerGrid)
    val resultMG2 = if (countBugs(innerGrid) == 0) resultMG1 else resultMG1 + (innerDepth -> innerGrid)

    resultMG2
  }

  private def calcNextTileState(tile: Tile, neighbourBugs: Int): Tile = {
    if (tile == TileBug) {
      if (neighbourBugs == 1) TileBug else TileEmpty
    } else {
      if (neighbourBugs == 1 || neighbourBugs == 2) TileBug else TileEmpty
    }
  }

  private def getGridTile(grid: Grid, coord: Coord): Tile = {
    val Point(x, y) = coord
    if (x < 0 || y < 0 || x >= GridSide || y >= GridSide) TileEmpty
    else grid(y)(x)
  }

  private def getMultiGridTile(multiGrid: MultiGrid, depth: Depth, coord: Coord): Tile = {
    multiGrid.get(depth).map(grid => getGridTile(grid, coord)).getOrElse(TileEmpty)
  }

  private def calcBiodiversity(grid: Grid): Int = {
    import adventOfCode.utils.algorithms.powInt
    grid.iterator.flatten.zipWithIndex.collect { case (TileBug, i) => powInt(2, i) }.sum
  }

  private def GridSide = 5
  private val GridCenter = GridSide / 2
  private val TileBug = '#'
  private val TileEmpty = '.'
  private val EmptyGrid: Grid = Array.tabulate(GridSide, GridSide) { case _ => TileEmpty }

}
