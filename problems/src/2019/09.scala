package adventOfCode.problems
package year2019

object problem09 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Long = {
    solve(input, 1L)
  }

  override def solve2(input: Input): Long = {
    solve(input, 2L)
  }

  private def solve(input: Input, programInput: Long): Long = {
    val memory = parseMemory(input)
    val machine = run(memory, List(programInput))

    machine.output.head
  }

}
