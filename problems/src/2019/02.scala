package adventOfCode.problems
package year2019

object problem02 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Int = {
    runPatchedCodes(parseMemory(input), noun = 12, verb = 2)
  }

  override def solve2(input: Input): Int = {
    val codes = parseMemory(input)
    val resultToFind = 19690720

    val solutions = for {
      noun <- (0 to 99).iterator
      verb <- (0 to 99).iterator
      if runPatchedCodes(codes, noun, verb) == resultToFind
    } yield 100 * noun + verb

    solutions.nextOption().getOrElse(sys.error("no solution"))
  }

  def runPatchedCodes(initialMemory: Memory, noun: Code, verb: Code): Code = {
    val patchedCodes = initialMemory.updated(1, noun).updated(2, verb)
    val finalState = run(patchedCodes)

    finalState.memory(0)
  }

  private[problems] def implTests(): Unit = {
    import utest._

    run(Vector(1, 0, 0, 0, 99)).memory ==> Vector(2, 0, 0, 0, 99)
    run(Vector(2, 3, 0, 3, 99)).memory ==> Vector(2, 3, 0, 6, 99)
    run(Vector(2, 4, 4, 5, 99, 0)).memory ==> Vector(2, 4, 4, 5, 99, 9801)
    run(Vector(1, 1, 1, 4, 99, 5, 6, 0, 99)).memory ==> Vector(30, 1, 1, 4, 2, 5, 6, 0, 99)
  }

}
