package adventOfCode.problems
package year2019

object problem18 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.neighbours4

  override def solve1(input: Input): Int = {
    solve(input, isMultiEntrance = false)
  }

  override def solve2(input: Input): Int = {
    solve(input, isMultiEntrance = true)
  }

  private def solve(input: Input, isMultiEntrance: Boolean): Int = {
    val entrances =
      if (isMultiEntrance) Vector(TileEntrance1, TileEntrance2, TileEntrance3, TileEntrance4)
      else Vector(TileEntrance)

    val grid = {
      val initialGrid = makeGrid(input)
      if (isMultiEntrance) splitEntrace(initialGrid) else initialGrid
    }

    val gridGraph = makeGridGraph(grid)
    val initialState = Map(State(emptyKeyBits, entrances) -> 0)

    calcMinSteps(initialState, gridGraph)
  }

  private type Coord = Point[Int]
  private type Grid = Array[String]
  private type Tile = Char
  private type Key = Tile
  private type KeyBits = Int
  private type StepsNum = Int

  private case class NextTile(tile: Tile, distance: StepsNum)
  private type AvailableTiles = List[NextTile]
  private type AvailableKeys = AvailableTiles

  private case class State(keyBits: KeyBits, robotLocations: Vector[Key])
  private type StateMap = Map[State, StepsNum]

  private type GridGraph = Map[Tile, AvailableTiles]


  private def calcMinSteps(initialStates: StateMap, gridGraph: GridGraph): StepsNum = {
    val statesIter = Iterator.iterate((initialStates, Int.MaxValue)){
      case (states: StateMap, minSteps: StepsNum) => {
        states.foldLeft((Map.empty: StateMap, minSteps)) {
          case ((statesAcc, minStepsAcc), (state, stepsSoFar)) => {
            val availableKeys = state.robotLocations.zipWithIndex.flatMap {
              case (location, robotIndex) => {
                bfsGraphScan(gridGraph, state.keyBits, location, stepsSoFar).map(_ -> robotIndex)
              }
            }

            if (availableKeys.isEmpty) { // no more keys available => all keys gathered
              (statesAcc, stepsSoFar min minStepsAcc)
            } else {
              val nextStates = mergeStates(statesAcc, state, availableKeys)
              (nextStates, minStepsAcc)
            }
          }
        }
      }
    }

    statesIter.collectFirst{ case (states, stepsNum) if states.isEmpty => stepsNum }.get
  }

  private def mergeStates(states: StateMap, state: State, availableKeys: Vector[(NextTile, Int)]): StateMap = {
    availableKeys.foldLeft(states) {
      case (statesAcc, (NextTile(key, stepsToKey), robotIndex)) => {
        val updatedKeyBag = addKey(state.keyBits, key)
        val nextState = state.copy(keyBits = updatedKeyBag, robotLocations = state.robotLocations.updated(robotIndex, key))

        statesAcc.updatedWith(nextState) {
          case None => Some(stepsToKey)
          case Some(oldSteps) => {
            Some(oldSteps min stepsToKey)
          }
        }
      }
    }
  }

  private def makeGridGraph(grid: Grid): GridGraph = {
    import scala.collection.immutable.Queue

    @scala.annotation.tailrec
    def scan(toVisit: Queue[(Coord, StepsNum)], visited: Set[Coord], tilesFound: AvailableTiles): AvailableTiles = {
      if (toVisit.isEmpty) tilesFound
      else {
        val ((coord, steps), restCoords) = toVisit.dequeue
        if (visited.contains(coord)) scan(restCoords, visited, tilesFound)
        else {
          val tile = getGridTile(grid, coord)
          val updatedVisited = visited.incl(coord)

          if ((isDoor(tile) || isKey(tile)) && steps != 0) {
            scan(restCoords, updatedVisited, NextTile(tile, steps) :: tilesFound)
          } else {
            val nextCoords = neighbours4(coord)
              .withFilter(neighbour => getGridTile(grid, neighbour) != TileWall && !visited.contains(neighbour))
              .map { case coord => (coord, steps + 1) }

            scan(restCoords ++ nextCoords, updatedVisited, tilesFound)
          }
        }
      }
    }

    findDoorsAndKeys(grid).map {
      case (doorOrKey, coord) => {
        val neighbourDoorsAndKeys = scan(Queue((coord, 0)), Set.empty, List.empty)
        doorOrKey -> neighbourDoorsAndKeys
      }
    }.toMap
  }

  private def bfsGraphScan(gridGraph: GridGraph, keyBits: KeyBits, location: Key, stepsSoFar: StepsNum): AvailableKeys = {
    import scala.collection.immutable.Queue

    def scan(toVisit: Queue[NextTile], visited: Set[Tile], keysFound: AvailableKeys): AvailableKeys = {
      if (toVisit.isEmpty) keysFound
      else {
        val (next, restTiles) = toVisit.dequeue
        val movableTiles = gridGraph(next.tile)
          .collect {
            case neighbour if (isKey(neighbour.tile) || hasKey(keyBits, neighbour.tile)) && !visited.contains(neighbour.tile) =>
              neighbour.copy(distance = neighbour.distance + next.distance)
          }

        val nextVisited = visited.incl(next.tile)

        val nextKeysFound = {
          if (isKey(next.tile) && !hasKey(keyBits, next.tile)) NextTile(next.tile, next.distance) :: keysFound
          else keysFound
        }

        scan(restTiles ++ movableTiles, nextVisited, nextKeysFound)
      }
    }

    scan(Queue(NextTile(location, stepsSoFar)), Set.empty, List.empty)
  }

  private def findDoorsAndKeys(grid: Grid): Iterator[(Tile, Coord)] = {
    val tiles = for {
      (line, y) <- grid.iterator.zipWithIndex
      (tile, x) <- line.iterator.zipWithIndex
    } yield (tile, Point(x, y))

    tiles.filter { case (tile, _) => isDoor(tile) || isKey(tile) || isEntrance(tile) }
  }

  private def findInitialEntrance(grid: Grid): Coord = {
    findDoorsAndKeys(grid).collectFirst { case (TileEntrance, coord) => coord }.get
  }

  private def getGridTile(grid: Grid, coord: Coord): Tile = {
    val height = grid.size
    val width = grid.head.size

    if (coord.x < 0 || coord.y < 0 || coord.y >= height || coord.x >= width) TileWall
    else grid(coord.y)(coord.x)
  }

  private def splitEntrace(grid: Grid): Grid = {
    val entranceCoord = findInitialEntrance(grid)

    val updates = List(
      (Point(-1, -1), TileEntrance1),
      (Point(1, -1), TileEntrance2),
      (Point(-1, 1), TileEntrance3),
      (Point(1, 1), TileEntrance4),
      (Point(-1, 0), TileWall),
      (Point(1, 0), TileWall),
      (Point(0, -1), TileWall),
      (Point(0, 1), TileWall),
      (Point(0, 0), TileWall)
    )

    updates.foldLeft(grid) { case (gridAcc, (coord, tile)) =>
      setGridTile(gridAcc, coord + entranceCoord, tile)
    }

  }

  private def makeGrid(input: Input): Grid = input.getLines().toArray

  private def setGridTile(grid: Grid, coord: Coord, tile: Tile): Grid = {
    grid.updated(coord.y, grid(coord.y).updated(coord.x, tile))
  }

  private def keyBit(key: Key): Int = 1 << (key - 'a')
  private def addKey(keyBits: KeyBits, key: Key): KeyBits = keyBits | keyBit(key)
  private def hasKey(keyBits: KeyBits, key: Key): Boolean = (keyBits | keyBit(key)) == keyBits
  private def emptyKeyBits: KeyBits = 0

  private def isDoor(tile: Tile): Boolean = tile.isUpper
  private def isKey(tile: Tile): Boolean = tile.isLower
  private def isEntrance(tile: Tile): Boolean =
    tile == TileEntrance || tile == TileEntrance1 || tile == TileEntrance2 || tile == TileEntrance3 || tile == TileEntrance4

  private val TileWall = '#'
  private val TileEntrance = '@'
  private val TileEntrance1 = '1'
  private val TileEntrance2 = '2'
  private val TileEntrance3 = '3'
  private val TileEntrance4 = '4'

}
