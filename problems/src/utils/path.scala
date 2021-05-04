package adventOfCode.utils

import adventOfCode.utils.geo.Point

package path {

  object Turn extends scala.Enumeration {
    val Left, Right, None = Value
    type Turn = Value
  }

  object Bearing extends scala.Enumeration {
    val North, East, South, West = Value
    type Bearing = Value
  }

  object Dir extends scala.Enumeration {
    val Up, Right, Down, Left = Value
    type Dir = Value
  }

  trait BasePath {
    type Instruction
    def startLocation: Location

    def locations(): Iterator[Location] = runInstructions(instructions())
    def stepByStepLocations(): Iterator[Location] = runInstructions(instructions().flatMap(makeStepByStepInstruction))

    protected def instructions(): Iterator[Instruction]
    protected def runInstructions(insructions: Iterator[Instruction]): Iterator[Location]
    protected def makeStepByStepInstruction(instruction: Instruction): Iterator[Instruction]
  }

  class TurnPath(input: String, override val startLocation: Location, startDir: Bearing.Bearing) extends BasePath {
    case class Instruction(turn: Turn.Turn, distance: Int)
    case class State(dir: Bearing.Bearing, location: Location)

    protected override def instructions(): Iterator[Instruction] = parseInstructions(input)

    protected override def runInstructions(instructions: Iterator[Instruction]): Iterator[Location] = {
      val startState = State(startDir, startLocation)
      val states = instructions.scanLeft(startState)(runInstruction)
      states.map(_.location)
    }

    protected override def makeStepByStepInstruction(instruction: Instruction): Iterator[Instruction] = {
      Iterator.single(Instruction(instruction.turn, 1)) ++
        Iterator.continually(Instruction(Turn.None, 1)).take(instruction.distance - 1)
    }

    private def runInstruction(state: State, instruction: Instruction): State = {
      val newDir =
        doTurn(state.dir, instruction.turn)
      val delta = bearingLocDelta(newDir, instruction.distance)

      State(newDir, state.location + delta)
    }

    private def parseInstructions(input: String): Iterator[Instruction] = {
      import fastparse._
      import SingleLineWhitespace._
      import adventOfCode.utils.parse.{num, parseValue}

      def turn[_: P] = P("L").map(_ => Turn.Left) | P("R").map(_ => Turn.Right)

      def instr[_: P] = P(turn ~ num).map {
        Instruction tupled _
      }

      def parser[_: P] = instr.rep(sep = ",")

      parseValue(input, parser(_)).iterator
    }
  }

  class DirPath(input: String, override val startLocation: Location) extends BasePath {
    case class Instruction(dir: Dir.Dir, distance: Int)
    case class State(location: Location)

    override protected def instructions(): Iterator[Instruction] = parseInstructions(input)
    override protected def makeStepByStepInstruction(instruction: Instruction): Iterator[Instruction] = {
      Iterator.continually(Instruction(instruction.dir, 1)).take(instruction.distance)
    }

    override protected def runInstructions(instructions: Iterator[Instruction]): Iterator[Location] = {
      val startState = State(startLocation)
      val states = instructions.scanLeft(startState)(runInstruction)
      states.map(_.location)
    }

    private def runInstruction(state: State, instruction: Instruction): State = {
      val delta = dirLocDelta(instruction.dir, instruction.distance)
      State(state.location + delta)
    }

    private def parseInstructions(input: String): Iterator[Instruction] = {
      import fastparse._
      import SingleLineWhitespace._
      import adventOfCode.utils.parse.{num, parseValue}

      def dir[_: P] =
        P("U").map(_ => Dir.Up) |
          P("R").map(_ => Dir.Right) |
          P("D").map(_ => Dir.Down) |
          P("L").map(_ => Dir.Left)

      def instr[_: P] = P(dir ~ num).map { Instruction tupled _ }
      def parser[_: P] = instr.rep(sep = ",")

      parseValue(input, parser(_)).iterator
    }
  }
}

package object path {

  import Turn.Turn
  import Bearing.Bearing
  import Dir.Dir

  type Location = geo.Point[Int]
  val Location = geo.Point[Int] _

  def neighbours8(l: Location): List[Location] = {
    List(
      l + UpLoc,
      l + DownLoc,
      l + RightLoc,
      l + LeftLoc,
      l + UpLoc + LeftLoc,
      l + UpLoc + RightLoc,
      l + DownLoc + LeftLoc,
      l + DownLoc + RightLoc
    )
  }

  def neighbours4(l: Location): List[Location] = {
    List(l + UpLoc, l + DownLoc, l + RightLoc, l + LeftLoc)
  }

  private def UpLoc = geo.Point(0, 1)
  private def DownLoc = geo.Point(0, -1)
  private def RightLoc = geo.Point(1, 0)
  private def LeftLoc = geo.Point(-1, 0)

  def manhattanDistance(from: Location, to: Location) = {
    (to.x - from.x).abs + (to.y - from.y).abs
  }

  def doTurn(currentDir: Bearing, turn: Turn, turns: Int): Bearing = {
    if (turn == Turn.None) currentDir
    else {
      val shift =
        if (turn == Turn.Left) -turns else +turns
      val dirs = Bearing.values.size

      Bearing((currentDir.id + shift + dirs) % dirs)
    }
  }

  def doTurn(currentDir: Bearing, turn: Turn): Bearing = {
    doTurn(currentDir, turn, 1)
  }

  def bearingLocDelta(bearing: Bearing, distance: Int = 1): Point[Int] = bearing match {
    case Bearing.North => Point(0, distance)
    case Bearing.East  => Point(distance, 0)
    case Bearing.South => Point(0, -distance)
    case Bearing.West  => Point(-distance, 0)
  }

  def bearingLocDeltaYInv(bearing: Bearing, distance: Int = 1): Point[Int] = {
    val delta = bearingLocDelta(bearing, distance)
    delta.copy(y = -delta.y)
  }

  def dirLocDelta(dir: Dir, distance: Int = 1): Point[Int] = {
    val bearingFromDir = dir match {
      case Dir.Up    => Bearing.North
      case Dir.Right => Bearing.East
      case Dir.Down  => Bearing.South
      case Dir.Left  => Bearing.West
    }
    bearingLocDelta(bearingFromDir, distance)
  }

}
