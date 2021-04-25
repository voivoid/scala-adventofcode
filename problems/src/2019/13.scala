package adventOfCode.problems
package year2019

object problem13 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Int = {
    val output = run(parseMemory(input)).output

    output
      .grouped(3)
      .count {
        case List(TileBlock, _, _) => true
        case _                     => false
      }
  }

  override def solve2(input: Input): Long = {
    val patchedMemory = parseMemory(input).updated(0, 2L)
    val startedGame = run(patchedMemory)

    val paddleX = getOutputTriples(startedGame).collectFirst { case List(TilePaddle, _, x) =>
      x
    }.get

    resumeGame(startedGame, paddleX)
  }

  @scala.annotation.tailrec
  private def resumeGame(machine: Machine, paddleX: Long): Long = {
    if (machine.waitsForInput) {
      val ballX = getOutputTriples(machine).collectFirst { case List(TileBall, _, x) =>
        x
      }.get

      val joystickInput = ballX.compare(paddleX)
      resumeGame(resumeMachine(resetOutput(machine), List(joystickInput.toLong)), paddleX + joystickInput)
    } else {
      getOutputTriples(machine).collectFirst { case List(n, 0, -1) =>
        n
      }.get
    }
  }

  private def getOutputTriples(machine: Machine) = {
    machine.output.grouped(3)
  }

  private val TileBlock = 2
  private val TilePaddle = 3
  private val TileBall = 4

}
