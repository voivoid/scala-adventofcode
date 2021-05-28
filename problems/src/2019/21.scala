package adventOfCode.problems
package year2019

object problem21 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Long = {
    solve(input, prog1)
  }

  override def solve2(input: Input): Long = {
    solve(input, prog2)
  }

  private def solve(input: Input, prog: String): Long = {
    val machineInput = (prog + "\n").toList.map(_.toLong)
    val machine = run(parseMemory(input.mkString), machineInput)
    machine.output.head
  }

  private def prog1 =
    """NOT A T
      |NOT B J
      |OR T J
      |NOT C T
      |OR T J
      |AND D J
      |WALK""".stripMargin

  private def prog2 =
    """NOT A T
      |NOT B J
      |OR T J
      |NOT C T
      |OR T J
      |AND D J
      |NOT E T
      |NOT T T
      |OR H T
      |AND T J
      |RUN""".stripMargin

}
