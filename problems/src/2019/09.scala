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

  private[problems] def implTests(): Unit = {
    import utest._

    val quineCodes = Vector[Long](109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99)

    run(quineCodes).output ==> quineCodes.reverse.toList

    run(Vector[Long](1102, 34915192, 34915192, 7, 4, 7, 99, 0)).output ==> List(1219070632396864L)
    run(Vector(104L, 1125899906842624L, 99L)).output ==> List(1125899906842624L)
  }

}
