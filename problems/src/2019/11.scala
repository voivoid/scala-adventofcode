package adventOfCode.problems
package year2019

import adventOfCode.utils.path.doTurn

object problem11 extends baseProblem {

  import adventOfCode.utils.intcode._
  import adventOfCode.utils.path.{Bearing, Turn, bearingLocDelta}
  import adventOfCode.utils.geo.Point

  override def solve1(input: Input): Int = {
    val paintedMap = runPaintProgram(input, Map.empty)
    paintedMap.size
  }

  override def solve2(input: Input): String = {
    val paintedMap = runPaintProgram(input, Map(StartCoord -> White))
    val paintCoords = paintedMap.keys

    import adventOfCode.utils.algorithms.IteratorMinMax
    val (minX, maxX) = paintCoords.iterator.map(_.x).minmax
    val (minY, maxY) = paintCoords.iterator.map(_.y).minmax

    val cols = paintCoords.groupBy(_.x)
    val rows = paintCoords.groupBy(_.y)

    // paintedMap may have redundant black tiles set by the painting robot that
    // may break a primitive OCR algorithm, so we need to be sure to filter them
    // by throwing away columns and rows that are out of the id rect

    val pixels = for {
      y <- maxY to minY by -1
      if rows(y).size >= CharWidth * IdLen
      x <- minX to maxX
      if cols(x).size >= CharHeight
    } yield paintedMap.getOrElse(Point(x, y), 0) match {
      case 0 => '0'
      case 1 => '1'
    }

    adventOfCode.utils.ocr.decodeChars(pixels, CharWidth * IdLen, CharHeight, CharWidth)
  }

  private def runPaintProgram(input: Input, initialMap: PaintMap): PaintMap = {
    val initialState = State(run(parseMemory(input)), Bearing.North, StartCoord, initialMap)
    val finalState = Iterator.iterate(initialState)(calcNextState).find(_.machine.halted).get

    finalState.paintedMap
  }

  private type Bearing = Bearing.Bearing
  private type Turn = Turn.Turn
  private type Coord = Point[Int]
  private type Color = Int
  private type PaintMap = Map[Coord, Color]
  private def StartCoord: Coord = Point(0, 0)
  private def Black: Color = 0
  private def White: Color = 1

  private def CharHeight = 6
  private def CharWidth = 5
  private def IdLen = 8

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
