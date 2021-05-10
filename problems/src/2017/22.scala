package adventOfCode.problems
package year2017

object problem22 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.{Bearing, bearingLocDelta, Turn, doTurn}

  override def solve1(input: Input): Int = {
    solve1(input, 10000)
  }

  private[problems] def solve1(input: Input, bursts: Int): Int = {
    solve(input, bursts, getNextNodeState1, Node.Clean)
  }

  override def solve2(input: Input): Int = {
    solve2(input, 10000000)
  }

  private[problems] def solve2(input: Input, bursts: Int): Int = {
    solve(input, bursts, getNextNodeState2, Node.Weakened)
  }

  private def solve(input: Input, bursts: Int, nextNode: Node.Node => Node.Node, nodeToCount: Node.Node): Int = {
    val (grid, startCoord) = parseInput(input)
    val initialState = State(grid, startCoord, Bearing.North, 0)

    import adventOfCode.utils.algorithms.IteratorLast
    val finalState = Iterator.iterate(initialState, bursts + 1)(runBurst(_, nextNode, nodeToCount)).last

    finalState.infected
  }

  private type Coord = Point[Int]
  private case class State(grid: NodeGrid, currentLoc: Coord, bearing: Bearing.Bearing, infected: Int)

  private object Node extends scala.Enumeration {
    val Clean, Weakened, Infected, Flagged = Value
    type Node = Value
  }
  private type NodeGrid = Array[Array[Node.Node]]

  private def runBurst(state: State, nextState: Node.Node => Node.Node, nodeToCount: Node.Node): State = {
    import state._

    val Point(x, y) = currentLoc
    val currentNode = grid(y)(x)
    grid(y)(x) = nextState(currentNode)

    val nextBearing = {
      val turn = currentNode match {
        case Node.Clean    => Turn.Left
        case Node.Weakened => Turn.None
        case Node.Infected => Turn.Right
        case Node.Flagged  => Turn.Reverse
      }
      doTurn(bearing, turn)
    }

    val (nextGrid, nextLoc) = {
      val nextLoc = currentLoc + bearingLocDelta(nextBearing)
      if (shouldInflate(grid, nextLoc)) inflate(grid, nextLoc)
      else (grid, nextLoc)
    }

    val nextInfected = infected + (if (currentNode == nodeToCount) 1 else 0)

    State(nextGrid, nextLoc, nextBearing, nextInfected)
  }

  private def getNextNodeState1(node: Node.Node): Node.Node = {
    if (node == Node.Clean) Node.Infected else Node.Clean
  }

  private def getNextNodeState2(node: Node.Node): Node.Node = {
    Node((node.id + 1) % Node.values.size)
  }

  private def shouldInflate(grid: NodeGrid, loc: Coord): Boolean = {
    loc.x < 0 || loc.y < 0 || loc.x >= grid.head.size || loc.y >= grid.size
  }

  private def inflate(grid: NodeGrid, loc: Coord): (NodeGrid, Coord) = {
    val width = grid.head.size
    val height = grid.size

    val nextGrid = Array.fill(height * 3, width * 3)(Node.Clean)

    for {
      y <- 0 until height
      x <- 0 until width
    } nextGrid(y + height)(x + width) = grid(y)(x)

    val nextLoc = loc + Point(width, height)

    (nextGrid, nextLoc)
  }

  private def parseInput(input: Input): (NodeGrid, Coord) = {
    val grid = input
      .getLines()
      .map(line =>
        line.map {
          case '#' => Node.Infected
          case '.' => Node.Clean
        }.toArray
      )
      .toArray

    val width = grid.head.size
    val height = grid.size

    val startX = width / 2
    val startY = height / 2

    (grid, Point(startX, startY))
  }

}
