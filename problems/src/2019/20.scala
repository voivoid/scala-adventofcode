package adventOfCode.problems
package year2019

object problem20 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import scala.collection.immutable.{TreeSet, Queue}
  import adventOfCode.utils.path.neighbours4
  import scala.collection.immutable.MultiDict

  override def solve1(input: Input): Int = {
    val portalMap = makePortalMap(input)
    calcMinSteps(portalMap, portalMap)
  }

  override def solve2(input: Input): Int = {
    val portalMap = makePortalMap(input)

    val startFloorMap =
      portalMap.filter { case (name, movement) =>
        (name == PortalEntrance || isInnerPortal(name)) && !isMoveToOuter(movement)
      }

    val restFloorsMap =
      portalMap.filter { case (portal, Movement(destinationPortal, _)) =>
        portal != PortalEntrance && destinationPortal != PortalEntrance && destinationPortal != PortalExit
      }

    calcMinSteps(startFloorMap, restFloorsMap)
  }

  private type Grid = Array[String]
  private type Coord = Point[Int]
  private type Tile = Char
  private type StepsNum = Int
  private type Floor = Int
  private type PortalName = (Char, Char)
  private type PortalMap = MultiDict[PortalName, Movement]

  private case class Portal(name: PortalName, coord: Coord)
  private case class Movement(destination: PortalName, stepsNum: StepsNum)
  private case class State(steps: StepsNum, floor: Floor, portalName: PortalName) extends Ordered[State] {
    import scala.math.Ordered.orderingToOrdered
    override def compare(that: State): Int = (steps, floor, portalName).compare((that.steps, that.floor, that.portalName))
  }

  private def calcMinSteps(firstFloorMap: PortalMap, restFloorsMap: PortalMap): StepsNum = {
    def step(toCheck: TreeSet[State], visited: Set[(Floor, PortalName)]): StepsNum = {
      val State(currentSteps, floor, portalName) = toCheck.head

      if (portalName == PortalExit) currentSteps
      else if (visited.contains((floor, portalName))) step(toCheck.tail, visited)
      else {
        val floorMap = if (floor == 0) firstFloorMap else restFloorsMap
        val neighbours = floorMap.get(portalName).map {
          case Movement(destination, steps) => {
            val nextFloor = floor + (if (isInnerPortal(destination)) 1 else -1)
            State(currentSteps + steps, nextFloor, destination)
          }
        }

        step(toCheck.tail ++ neighbours, visited.incl((floor, portalName)))
      }
    }

    val initialState = State(0, 0, PortalEntrance)

    // when ZZ is reached there is no need to waste a step to go through it so decrement 1 from the number of total steps
    step(TreeSet(initialState), Set.empty) - 1
  }

  private def makePortalMap(input: Input): PortalMap = {
    val grid = makeGrid(input)
    makePortalMap(grid)
  }

  private def makePortalMap(grid: Grid): PortalMap = {
    val (allOuter, inner) = findPortals(grid)

    val (aazzOuter, restOuter) =
      allOuter.partition { case Portal(name, _) => name == PortalEntrance || name == PortalExit }

    val Vector(entrance, exit) = aazzOuter.sortBy(_.name): @unchecked

    val lowerCaseInner = inner.map { case p @ Portal((c1, c2), _) => p.copy(name = (c1.toLower, c2.toLower)) }
    val pairedPortals = restOuter ++ lowerCaseInner

    val gridWithPortalChars = (pairedPortals :+ exit).foldLeft(grid) { case (gridAcc, portal: Portal) =>
      setGridTile(gridAcc, portalNameToTile(portal.name), portal.coord)
    }

    (pairedPortals :+ entrance)
      .flatMap(portal => bfsScan(gridWithPortalChars, portal).map(portal.name -> _))
      .to(MultiDict)
  }

  private def bfsScan(gridWithPortals: Grid, portal: Portal): List[Movement] = {
    @scala.annotation.tailrec
    def step(coordsToVisit: Queue[(Coord, StepsNum)], visited: Set[Coord], portalsFound: List[Movement]): List[Movement] = {
      if (coordsToVisit.isEmpty) portalsFound
      else {
        val ((coord, steps), restCoords) = coordsToVisit.dequeue
        val tile = getGridTile(gridWithPortals, coord)

        if (visited.contains(coord)) step(restCoords, visited, portalsFound)
        else if (steps != 0 && isPortalTile(tile)) {
          val destinationPortalName = tileToPortalName(tile) match {
            case PortalExit => PortalExit
            case (c1, c2)   => if (c1.isLower) (c1.toUpper, c2.toUpper) else (c1.toLower, c2.toLower)
          }

          // +1 step to make a jump through a portal
          step(restCoords, visited + coord, Movement(destinationPortalName, steps + 1) :: portalsFound)
        } else {
          val neighbours = neighbours4(coord)
            .filter(c => {
              val tile = getGridTile(gridWithPortals, c)
              tile == TilePassage || isPortalTile(tile)
            })
            .map(c => (c, steps + 1))

          step(restCoords ++ neighbours, visited + coord, portalsFound)
        }
      }
    }

    step(Queue((portal.coord, 0)), Set.empty, List.empty)
  }

  private def makeGrid(input: Input): Grid = input.getLines().toArray

  private def setGridTile(grid: Grid, tile: Tile, coord: Coord): Grid = {
    grid.updated(coord.y, grid(coord.y).updated(coord.x, tile))
  }

  private def findPortals(grid: Grid): (Vector[Portal], Vector[Portal]) = {
    val width = grid(0).length
    val height = grid.length
    val center = Point(height / 2, width / 2)

    def findDonutEdge(from: Int, step: Int, makeCoord: Int => Coord, findEmpty: Boolean): Int =
      Iterator.from(from, step).find(c => findEmpty == isEmptyTile(getGridTile(grid, makeCoord(c)))).get

    def findDonutEdges(from: Int, step: Int, makeCoord: Int => Coord): (Int, Int) = {
      val e1 = findDonutEdge(from, step, makeCoord, false)
      val e2 = findDonutEdge(e1, step, makeCoord, true) - step
      (e1, e2)
    }

    def findHorEdges(from: Int, step: Int): (Int, Int) = findDonutEdges(from, step, Point(_, center.y))
    def findVerEdges(from: Int, step: Int): (Int, Int) = findDonutEdges(from, step, Point(center.x, _))

    def getEdgePortals(left: Int, right: Int, top: Int, bottom: Int, inner: Boolean) = {
      val bottomCoords = (left to right).map(Point(_, bottom))
      val topCoords = (left to right).map(Point(_, top))
      val rightCoords = (top to bottom).map(Point(right, _))
      val leftCoords = (top to bottom).map(Point(left, _))

      def collect(coords: Seq[Coord], readPortalName: (Grid, Coord) => PortalName, nameShift: Coord) =
        coords.collect {
          case coord if getGridTile(grid, coord) == TilePassage => {
            Portal(readPortalName(grid, coord + nameShift), coord)
          }
        }
      def collectHor(coords: Seq[Coord], nameShift: Coord) = collect(coords, readHorizontalPortal, nameShift)
      def collectVer(coords: Seq[Coord], nameShift: Coord) = collect(coords, readVerticalPortal, nameShift)

      collectVer(bottomCoords, if (inner) Point(0, -2) else Point(0, 1)) ++
        collectVer(topCoords, if (inner) Point(0, 1) else Point(0, -2)) ++
        collectHor(leftCoords, if (inner) Point(1, 0) else Point(-2, 0)) ++
        collectHor(rightCoords, if (inner) Point(-2, 0) else Point(1, 0))
    }

    val (innerLeft, outerLeft) = findHorEdges(center.x, -1)
    val (innerRight, outerRight) = findHorEdges(center.x, +1)
    val (innerTop, outerTop) = findVerEdges(center.y, -1)
    val (innerBottom, outerBottom) = findVerEdges(center.y, +1)

    val innerPortals = getEdgePortals(innerLeft, innerRight, innerTop, innerBottom, true).toVector
    val outerPortals = getEdgePortals(outerLeft, outerRight, outerTop, outerBottom, false).toVector

    (outerPortals, innerPortals)
  }

  private def readVerticalPortal(grid: Grid, coord: Coord): PortalName = {
    readPortal(grid, coord, Point(0, 1))
  }

  private def readHorizontalPortal(grid: Grid, coord: Coord): PortalName = {
    readPortal(grid, coord, Point(1, 0))
  }

  private def readPortal(grid: Grid, coord: Coord, c2Shift: Coord): PortalName = {
    val c1 = getGridTile(grid, coord)
    val c2 = getGridTile(grid, coord + c2Shift)
    assert(c1.isUpper && c2.isUpper)
    (c1, c2)
  }

  private def getGridTile(grid: Grid, coord: Coord): Tile = {
    grid(coord.y)(coord.x)
  }

  private def isEmptyTile(tile: Tile): Boolean = tile != TileWall && tile != TilePassage

  private def portalNameToTile(pn: PortalName): Tile = ((pn._1 << 8) | pn._2).toChar
  private def tileToPortalName(tile: Tile): PortalName = ((tile >> 8).toChar, (tile & 255).toChar)
  private def isPortalTile(tile: Tile): Boolean = tile >= 256

  private def isInnerPortal(name: PortalName): Boolean = name._1.isLower
  private def isMoveToOuter(movement: Movement): Boolean = isInnerPortal(movement.destination)

  private val TileWall = '#'
  private val TilePassage = '.'

  private val PortalEntrance = ('A', 'A')
  private val PortalExit = ('Z', 'Z')

}
