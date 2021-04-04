package adventOfCode.problems
package year2019

object problem05 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Long = {
    val output = runCodeAndGetOutput(input, inputCode = 1)
    output.find(_ != 0).getOrElse(sys.error("unexpected result"))
  }

  override def solve2(input: Input): Long = {
    val output = runCodeAndGetOutput(input, inputCode = 5)
    output.headOption.getOrElse(sys.error("unexpected result"))
  }

  private def runCodeAndGetOutput(input: Input, inputCode: Code): Out = {
    val codes = parseMemory(input)
    val finalState = run(codes, List(inputCode))

    finalState.output
  }

}
