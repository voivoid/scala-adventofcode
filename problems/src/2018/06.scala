package adventOfCode.problems
package year2018

object problem06 extends baseProblem {

  import adventOfCode.utils.geo.{Point, Rect}
  import adventOfCode.utils.algorithms.IteratorGroupValuesByTheirLength

  // both solve1 and solve2 are implemented using an O(W*H) algorithm instead of a naive O(W*H*C)
  // where W - border width
  //       H - border height
  //       C - number of input coords
  override def solve1(input: Input): Int = {
    val locationCoords = parseCoords(input)
    val border = getBorder(locationCoords)

    val initialGrid = Array.fill[Int](border.height, border.width)(Empty)
    val shiftedCoordsWithIds = locationCoords.map(_ - border.leftTop).zipWithIndex.toSet

    val (filledGrid, _) = Iterator
      .iterate((initialGrid, shiftedCoordsWithIds)) { case (grid, coords) => fillGrid(grid, coords) }
      .find { case (_, coords) => coords.isEmpty }
      .get

    val gridBorderCoords = getBorderCoords(filledGrid)
    val gridBorderIds = gridBorderCoords.map { case Point(x, y) => filledGrid(y)(x) }.toSet
    val finiteGridIds = filledGrid.iterator.flatten.filter(!gridBorderIds.contains(_))

    val areas = finiteGridIds.groupValuesByTheirLength.values
    areas.max
  }

  override def solve2(input: Input): Int = {
    solve2(input, maxDistance = 10000)
  }

  def solve2(input: Input, maxDistance: Int): Int = {

    val locationCoords = parseCoords(input)
    val locationsNum = locationCoords.size

    val maxDistanceFromAnyLoc = maxDistance / locationCoords.size
    val border = getBorder(locationCoords).inflate(maxDistanceFromAnyLoc)

    // border coords: 0    1    2    3    4    5    6    7
    // locations:               A              B
    // (prev, next): 0,2  0,2  0,2  1,1  1,1  1,1  2,0  2,0
    // delta:        -2   -2   -2    0    0    0    2    2
    // dists:         7    5    3    3    3    3    5    7
    // dist change:     -2   -2   0    0    0    2    2
    //
    // next = a number of locations with axis coords >= current border coord
    // prev = a number of locations with axis coords < current border coord
    // delta = prev - next
    // dists = a sum of distances to A and B locs from current border coord
    //
    // notice a correlation between deltas and distance changes
    // so there is no need to recalculate sum of distances for each coordinate in O(n)
    // precalculated xs and ys deltas can be used to get the desired distances sum for any point in O(1)

    def calcDistanceDeltas(borderAxisCoords: Range, locationAxisCoords: List[Int]): List[Int] = {
      import adventOfCode.utils.algorithms.IteratorGroupValuesByTheirLength
      val axisCoordsMap = locationAxisCoords.iterator.groupValuesByTheirLength

      List.unfold((borderAxisCoords, 0)) {
        case (borderCoords, prevLocs) => {
          if (borderCoords.isEmpty) None
          else {
            val nextLocs = locationsNum - prevLocs
            val delta = prevLocs - nextLocs

            Some((delta, (borderCoords.tail, prevLocs + axisCoordsMap.getOrElse(borderCoords.head, 0))))
          }
        }
      }
    }

    val borderLeftCoords = border.leftTop.x to border.rightBottom.x
    val borderTopCoords = border.leftTop.y to border.rightBottom.y

    val xsDeltas = calcDistanceDeltas(borderLeftCoords, locationCoords.map(_.x))
    val ysDeltas = calcDistanceDeltas(borderTopCoords, locationCoords.map(_.y))

    val leftTopDistance = sumDistances(border.leftTop - Point(1, 1), locationCoords)

    val distancesInsideBorderRect = for {
      dx <- xsDeltas.iterator.scanLeft(0)(_ + _).drop(1)
      dy <- ysDeltas.iterator.scanLeft(0)(_ + _).drop(1)
    } yield leftTopDistance + dx + dy

    distancesInsideBorderRect.count(_ < maxDistance)
  }

  private type Coord = Point[Int]
  private type Id = Int
  private type Coords = List[Coord]
  private type CoordsWithId = Set[(Coord, Id)]
  private type Border = Rect[Int]
  private type Grid = Array[Array[Id]]

  private val Empty: Id = -1
  private val Contested: Id = -2

  private def sumDistances(p: Coord, coords: Coords): Int = {
    import adventOfCode.utils.path.manhattanDistance

    coords.map(manhattanDistance(_, p)).sum
  }

  private def parseCoords(input: Input): Coords = {
    input
      .getLines()
      .map(line => {
        val Array(x, y) = line.split(", ")
        Point(x.toInt, y.toInt)
      })
      .toList
  }

  private def getBorder(coords: Coords): Border = {
    import adventOfCode.utils.algorithms.IteratorMinMax
    val (left, right) = coords.iterator.map(_.x).minmax
    val (top, bottom) = coords.iterator.map(_.y).minmax

    Rect(Point(left, top), Point(right, bottom))
  }

  private def fillGrid(grid: Grid, coords: CoordsWithId): (Grid, CoordsWithId) = {
    coords.foreach { case (Point(x, y), id) =>
      val prevId = grid(y)(x)
      val idToSet = if (prevId == Empty) id else Contested
      grid(y)(x) = idToSet
    }

    val nextCoords = coords.flatMap { case (point, id) =>
      neighbours(point, grid).map((_, id))
    }

    (grid, nextCoords)
  }

  private def neighbours(p: Coord, grid: Grid): Coords = {
    val width = grid(0).length
    val height = grid.length

    def isValidPoint(p: Coord): Boolean = {
      val Point(x, y) = p
      x >= 0 && y >= 0 && x < width && y < height && grid(y)(x) == Empty
    }

    adventOfCode.utils.path.neighbours4(p).filter(isValidPoint)
  }

  private def getBorderCoords(grid: Grid): IndexedSeq[Coord] = {
    val width = grid(0).length
    val height = grid.length

    val xs = (0 until width)
    val ys = (0 until height)

    val top = xs.map(Point(_, 0))
    val bottom = xs.map(Point(_, height - 1))
    val left = ys.map(Point(0, _))
    val right = ys.map(Point(width - 1, _))

    left ++ top ++ right ++ bottom
  }

}
