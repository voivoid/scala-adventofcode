package adventOfCode.problems
package year2016

object problem01 extends baseProblem {

  override def solve1(input: Input): Int = {
    val instructions = parseInstructions(input.mkString)
    val finish = instructions.foldLeft(startState)(runInstruction)

    distance(startState.location, finish.location)
  }

  override def solve2(input: Input): Int = {
    val instructions = parseInstructions(input.mkString)

    val stepByStepInstructions =
      instructions.iterator.flatMap { case Instruction(turn, distance) =>
        Iterator
          .single(Instruction(turn, 1))
          .concat(
            Iterator
              .continually(Instruction(Turn.None, 1))
              .take(distance - 1)
          )
      }

    val pathHistory =
      stepByStepInstructions.scanLeft((Set.empty[Location], startState)) {
        case ((visited, currentState), instruction) => {
          val nextState = runInstruction(currentState, instruction)
          (visited + currentState.location, nextState)
        }
      }

    val (_, firstIntersection) = pathHistory
      .find { case (visited, state) =>
        visited(state.location)
      }
      .getOrElse(sys.error("Solution not found"))
    distance(startState.location, firstIntersection.location)
  }

  private def startState =
    State(Dir.North, Location(0, 0))

  private object Turn extends scala.Enumeration {
    val Left, Right, None = Value
    type Turn = Value
  }

  private object Dir extends scala.Enumeration {
    val North, East, South, West = Value
    type Dir = Value
  }

  import Turn.Turn, Dir.Dir

  private case class Instruction(turn: Turn, distance: Int)

  private case class State(dir: Dir, location: Location)

  private case class Location(x: Int, y: Int)

  private def parseInstructions(input: String): Seq[Instruction] = {
    import fastparse._
    import SingleLineWhitespace._
    import adventOfCode.utils.parse.{num, parseValue}

    def turn[_: P] = P("L").map(_ => Turn.Left) | P("R").map(_ => Turn.Right)

    def instr[_: P] = P(turn ~ num).map { Instruction tupled _ }

    def parser[_: P] = instr.rep(sep = ",")

    parseValue(input, parser(_))
  }

  private def distance(from: Location, to: Location) = {
    (to.x - from.x).abs + (to.y - from.x).abs
  }

  private def doTurn(currentDir: Dir, turn: Turn): Dir = {
    if (turn == Turn.None) currentDir
    else {
      val shift =
        if (turn == Turn.Left) -1 else +1
      val dirs = Dir.values.size

      Dir((currentDir.id + shift + dirs) % dirs)
    }
  }

  private def posDelta(dir: Dir, distance: Int) =
    dir match {
      case Dir.North => (0, distance)
      case Dir.East  => (distance, 0)
      case Dir.South => (0, -distance)
      case Dir.West  => (-distance, 0)
    }

  private def runInstruction(state: State, instruction: Instruction): State = {
    val newDir =
      doTurn(state.dir, instruction.turn)
    val (dx, dy) =
      posDelta(newDir, instruction.distance)

    State(newDir, Location(state.location.x + dx, state.location.y + dy))
  }

  def implTests(): Unit = {
    import utest._

    doTurn(Dir.North, Turn.Left) ==> Dir.West
    doTurn(Dir.North, Turn.Right) ==> Dir.East
    doTurn(Dir.West, Turn.Left) ==> Dir.South
    doTurn(Dir.West, Turn.Right) ==> Dir.North

    assertMatch(parseInstructions("R2, L3")) { case Seq(Instruction(Turn.Right, 2), Instruction(Turn.Left, 3)) =>
    }
  }
}
