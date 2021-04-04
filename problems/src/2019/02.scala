package adventOfCode.problems
package year2019

object problem02 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Long = {
    runPatchedCodes(parseMemory(input), noun = 12, verb = 2)
  }

  override def solve2(input: Input): Long = {
    val codes = parseMemory(input)
    val resultToFind = 19690720

    val solutions = for {
      noun <- (0L to 99L).iterator
      verb <- (0L to 99L).iterator
      if runPatchedCodes(codes, noun, verb) == resultToFind
    } yield 100 * noun + verb

    solutions.nextOption().getOrElse(sys.error("no solution"))
  }

  def runPatchedCodes(initialMemory: Memory, noun: Code, verb: Code): Code = {
    val patchedCodes = initialMemory.updated(1, noun).updated(2, verb)
    val finalState = run(patchedCodes)

    finalState.memory(0)
  }

}
