package adventOfCode.problems
package year2019

object problem15 extends baseProblem {

  import adventOfCode.utils.intcode._
  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.{Bearing, bearingLocDelta}
  import scala.collection.immutable.Queue

  override def solve1(input: Input): Int = {
    bfsFindOxygenLocation(input).distanceFromStart
  }

  override def solve2(input: Input): Int = {
    val initialState = bfsFindOxygenLocation(input).copy(distanceFromStart = 0)
    bfsMaxDistance(Queue(initialState), Set.empty, 0)
  }

  private type Coord = Point[Int]
  private type MoveCmd = Long
  private case class MachineState(machine: Machine, location: Coord, distanceFromStart: Int)

  private def bfsFindOxygenLocation(input: Input): MachineState = {
    val initialMachine = makeMachine(parseMemory(input))
    val initialState = MachineState(initialMachine, Point(0, 0), 0)
    findOxygenLocation(Queue(initialState), Set.empty).getOrElse(sys.error("no solution"))
  }

  @scala.annotation.tailrec
  private def findOxygenLocation(toVisit: Queue[MachineState], visited: Set[Coord]): Option[MachineState] = {
    if (toVisit.isEmpty) None
    else {
      val (state @ MachineState(machine, coord, distance), restToVisit) = toVisit.dequeue
      if (isOxygenFound(machine)) Some(state)
      else {
        val nextToVisit = calcNextToVisit(coord, visited, machine, distance)
        findOxygenLocation(restToVisit ++ nextToVisit, visited + coord)
      }
    }
  }

  @scala.annotation.tailrec
  private def bfsMaxDistance(toVisit: Queue[MachineState], visited: Set[Coord], maxDistance: Int): Int = {
    if (toVisit.isEmpty) maxDistance
    else {
      val (MachineState(machine, coord, distance), restToVisit) = toVisit.dequeue
      val nextToVisit = calcNextToVisit(coord, visited, machine, distance)
      bfsMaxDistance(restToVisit ++ nextToVisit, visited + coord, distance max maxDistance)
    }
  }

  private def isOxygenFound(machine: Machine): Boolean = {
    val output = machine.output
    !output.isEmpty && output.head == OutputOxygen
  }

  private def calcNextToVisit(coord: Coord, visited: Set[Coord], machine: Machine, distance: Int): List[MachineState] = {
    val neighbours = getNeighbours(coord, visited)

    val neighbourStates = neighbours.map {
      case (moveCmd, neighbourCoord) => {
        MachineState(resumeMachine(machine, List(moveCmd)), neighbourCoord, distance + 1)
      }
    }.toList

    neighbourStates.filter(_.machine.output.head != OutputWall)
  }

  private def getNeighbours(coord: Coord, visited: Set[Coord]): Iterator[(MoveCmd, Coord)] = {
    Bearings.iterator
      .map(bearing => (bearing, coord + bearingLocDelta(bearing)))
      .collect { case (bearing, coord) if !visited.contains(coord) => (bearingToMoveCmd(bearing), coord) }
  }

  private def bearingToMoveCmd(bearing: Bearing.Bearing): MoveCmd = bearing match {
    case Bearing.North => 1
    case Bearing.South => 2
    case Bearing.West  => 3
    case Bearing.East  => 4
  }

  val Bearings = List(Bearing.North, Bearing.East, Bearing.West, Bearing.South)

  val OutputWall = 0
  val OutputFree = 1
  val OutputOxygen = 2

}
