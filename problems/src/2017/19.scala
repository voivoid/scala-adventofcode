package adventOfCode.problems
package year2017

import adventOfCode.utils.path.bearingLocDeltaYInv

object problem19 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.{Bearing, Turn, doTurn}

  override def solve1(input: Input): String = {
    val grid = makeGrid(input)
    val path = getPath(grid)

    path.map(state => getGridLine(grid, state.loc)).filter(_.isLetter).mkString
  }

  override def solve2(input: Input): Int = {
    getPath(makeGrid(input)).size
  }

  private case class State(loc: Coord, bearing: Bearing.Bearing)
  private type Line = Char
  private type Coord = Point[Int]
  private type Grid = Array[Array[Line]]

  private def makeGrid(input: Input): Grid = {
    val lines = input.getLines()
    lines.map(_.toArray).toArray
  }

  private def getPath(grid: Grid): Iterator[State] = {
    val startX = grid.head.indexOf(VerLine)
    val initialState = State(Point(startX, 0), Bearing.South)

    Iterator.iterate(initialState)(runStep(grid, _)).takeWhile { case State(coord, _) => getGridLine(grid, coord) != NoLine }
  }

  private def runStep(grid: Grid, state: State): State = {
    import state._

    val line = getGridLine(grid, loc)
    if (line != CrossLine) {
      val nextLoc = loc + bearingLocDeltaYInv(bearing, 1)
      copy(loc = nextLoc)
    } else {
      val leftTurnBearing = doTurn(state.bearing, Turn.Left)
      val rightTurnBearing = doTurn(state.bearing, Turn.Right)

      val leftTurnLoc = loc + bearingLocDeltaYInv(leftTurnBearing)
      val rightTurnLoc = loc + bearingLocDeltaYInv(rightTurnBearing)

      if (isValidBearing(leftTurnBearing, getGridLine(grid, leftTurnLoc))) {
        State(leftTurnLoc, leftTurnBearing)
      } else {
        State(rightTurnLoc, rightTurnBearing)
      }
    }
  }

  private def isValidBearing(bearing: Bearing.Bearing, line: Char): Boolean = {
    line.isLetter || (
      if (bearing == Bearing.West || bearing == Bearing.East)
        line == HorLine
      else
        line == VerLine
    )
  }

  private def getGridLine(grid: Grid, coord: Coord): Char = {
    grid(coord.y)(coord.x)
  }

  private val CrossLine = '+'
  private val NoLine = ' '
  private val HorLine = '-'
  private val VerLine = '|'

}
