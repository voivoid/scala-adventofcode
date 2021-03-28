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

  private[problems] def implTests(): Unit = {
    import utest._
    run(Vector(3, 0, 4, 0, 99), List(123)).output ==> List(123)
    run(Vector(1002, 4, 3, 4, 33)).memory ==> Vector(1002, 4, 3, 4, 99)
    run(Vector(1101, 100, -1, 4, 0)).memory ==> Vector(1101, 100, -1, 4, 99)

    val isInputEqto8PosMode = Vector[Long](3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)
    run(isInputEqto8PosMode, List(8)).output ==> List(1)
    run(isInputEqto8PosMode, List(9)).output ==> List(0)

    val isInputLessThen8PosMode = Vector[Long](3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)
    run(isInputLessThen8PosMode, List(7)).output ==> List(1)
    run(isInputLessThen8PosMode, List(8)).output ==> List(0)

    val isInputEqto8ImMode = Vector[Long](3, 3, 1108, -1, 8, 3, 4, 3, 99)
    run(isInputEqto8ImMode, List(8)).output ==> List(1)
    run(isInputEqto8ImMode, List(9)).output ==> List(0)

    val isInputLessThen8ImMode = Vector[Long](3, 3, 1107, -1, 8, 3, 4, 3, 99)
    run(isInputLessThen8ImMode, List(7)).output ==> List(1)
    run(isInputLessThen8ImMode, List(8)).output ==> List(0)

    val zeroIfInputIsZeroPosMode = Vector[Long](3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)
    run(zeroIfInputIsZeroPosMode, List(0)).output ==> List(0)
    run(zeroIfInputIsZeroPosMode, List(1)).output ==> List(1)

    val zeroIfInputIsZeroImMode = Vector[Long](3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1)
    run(zeroIfInputIsZeroImMode, List(0)).output ==> List(0)
    run(zeroIfInputIsZeroImMode, List(1)).output ==> List(1)

    val cmpWith8 = Vector[Long](3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125,
      20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99)

    run(cmpWith8, List(7)).output ==> List(999)
    run(cmpWith8, List(8)).output ==> List(1000)
    run(cmpWith8, List(9)).output ==> List(1001)
  }

}
