package adventOfCode.problems
package year2015

object problem01 extends baseProblem {

  def solve1(input: Input): Int = getShifts(input).sum
  def solve2(input: Input): Int = getShifts(input).scanLeft(0)(_ + _).indexOf(-1)

  private def getShifts(input: Input) = input.filter(!_.isWhitespace).map(shift)
  private def shift(c: Char): Int = c match {
    case '(' => +1
    case ')' => -1
    case _ => sys.error("Unexpected input instruction")
  }

}
