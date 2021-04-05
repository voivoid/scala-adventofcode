package adventOfCode.problems
package year2019

import adventOfCode.utils.path.doTurn

object problem11 extends baseProblem {

  import adventOfCode.utils.intcode._
  import adventOfCode.utils.path.{Bearing, Turn, bearingLocDelta}
  import adventOfCode.utils.geo.Point

  override def solve1(input: Input): Int = {
    val initialState = State(run(parseMemory(input)), Bearing.North, Point(0, 0), Map.empty)
    val finalState = Iterator.iterate(initialState)(calcNextState).find(_.machine.halted).get

    finalState.paintedMap.size
  }

  override def solve2(input: Input): Int = {
    ???
  }

  private type Bearing = Bearing.Bearing
  private type Turn = Turn.Turn
  private type Coord = Point[Int]
  private type Color = Int
  private type PaintMap = Map[Coord, Color]
  private def Black: Color = 0

  private case class State(machine: Machine, bearing: Bearing, coord: Coord, paintedMap: PaintMap)

  private def calcNextState(state: State): State = {
    import state._
    assert(machine.waitsForInput)

    val currentPanelColor = paintedMap.getOrElse(coord, Black)
    val nextMachine = resumeMachine(machine, List(currentPanelColor.toLong))

    val turnCode :: colorToPaint :: _ = nextMachine.output: @unchecked

    val nextBearing = doTurn(bearing, makeTurn(turnCode))
    val nextPaintedMap = paintedMap.updated(coord, colorToPaint.toInt)

    val (dx, dy) = bearingLocDelta(nextBearing, 1)
    val nextCoord = coord + Point(dx, dy)

    State(nextMachine, nextBearing, nextCoord, nextPaintedMap)
  }

  private def makeTurn(turnCode: Code): Turn = {
    if (turnCode == 0) Turn.Left else Turn.Right
  }

}
