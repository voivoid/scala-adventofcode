package adventOfCode.problems
package year2020

object problem12 extends baseProblem {

  import adventOfCode.utils.path
  import adventOfCode.utils.path.{Bearing, Turn => PathTurn}
  import adventOfCode.utils.geo.Point

  override def solve1(input: Input): Int = {
    val actions = input.getLines().map(parseAction)

    val initialState = BearingState(Bearing.East, shipStartLocation)
    val finalState = actions.foldLeft(initialState)(runAction)

    path.manhattanDistance(shipStartLocation, finalState.shipCoord)
  }

  override def solve2(input: Input): Int = {
    val actions = input.getLines().map(parseAction)

    val initialWaypoint = Point(10, 1)
    val initialState = WaypointState(initialWaypoint, shipStartLocation)
    val finalState = actions.foldLeft(initialState)(runAction)

    path.manhattanDistance(shipStartLocation, finalState.shipCoord)
  }

  private type Coord = Point[Int]

  private sealed trait Action
  private case class Forward(n: Int) extends Action
  private case class Turn(turn: PathTurn.Turn, n: Int) extends Action
  private case class Move(bearing: Bearing.Bearing, n: Int) extends Action

  private case class BearingState(bearing: Bearing.Bearing, shipCoord: Coord)
  private case class WaypointState(waypointCoord: Coord, shipCoord: Coord)

  private def shipStartLocation = Point(0, 0)

  private def parseAction(s: String): Action = {
    val (action, valueStr) = s.splitAt(1)
    val v = valueStr.toInt

    action match {
      case "N" => Move(Bearing.North, v)
      case "S" => Move(Bearing.South, v)
      case "E" => Move(Bearing.East, v)
      case "W" => Move(Bearing.West, v)
      case "L" => assert(v % 90 == 0); Turn(PathTurn.Left, v % 360)
      case "R" => assert(v % 90 == 0); Turn(PathTurn.Right, v % 360)
      case "F" => Forward(v)
    }
  }

  private def runAction(state: BearingState, action: Action): BearingState = action match {
    case Forward(n) => {
      val delta = path.bearingLocDeltaYInv(state.bearing, n)
      state.copy(shipCoord = state.shipCoord + delta)
    }
    case Turn(turnDir, degree) => {
      val nextBearing = path.doTurn(state.bearing, turnDir, degree / 90)
      state.copy(bearing = nextBearing)
    }

    case Move(bearing, distance) => {
      val delta = path.bearingLocDeltaYInv(bearing, distance)
      state.copy(shipCoord = state.shipCoord + delta)
    }
  }

  private def runAction(state: WaypointState, action: Action): WaypointState = action match {
    case Forward(n) => {
      val (dx, dy) = (state.waypointCoord.x * n, state.waypointCoord.y * n)
      state.copy(shipCoord = state.shipCoord + Point(dx, dy))
    }

    case Turn(turn, degree) => {
      val turnDegree = if (turn == PathTurn.Left) degree else 360 - degree

      state.copy(waypointCoord = turnWaypointLeft(state.waypointCoord, turnDegree))
    }

    case Move(bearing, distance) => {
      val delta = path.bearingLocDeltaYInv(bearing, distance)
      state.copy(waypointCoord = state.waypointCoord + delta)
    }
  }

  private def turnWaypointLeft(waypointCoord: Coord, degree: Int): Coord = {
    val x = waypointCoord.x
    val y = waypointCoord.y

    degree match {
      case 0   => waypointCoord
      case 90  => Point(-y, x)
      case 180 => Point(-x, -y)
      case 270 => Point(y, -x)
    }
  }

}
