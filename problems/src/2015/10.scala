package adventOfCode.problems
package year2015

object problem10 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, iterations = 40)
  }

  override def solve2(input: Input): Int = {
    solve(input, iterations = 50)
  }

  def solve(input: Input, iterations: Int): Int = {
    import adventOfCode.utils.algorithms.IteratorLast

    val finalNumStr = Iterator.iterate(input.mkString, iterations + 1)(lookAndSay).last
    finalNumStr.size
  }

  private def asDigit(i: Int): Char = {
    assert(i >= 0 && i <= 9)
    ('0' + i).toChar
  }

  private def lookAndSay(s: String): String = {
    assert(!s.isEmpty)

    def appendDigit(acc: StringBuilder, digit: Char, counter: Int) = acc.append(asDigit(counter)).append(digit)

    val (acc, digit, counter) =
      s.foldLeft((new StringBuilder(), s.head, 0)) { case ((acc, currentDigit, counter), digit) =>
        if (digit == currentDigit) (acc, currentDigit, counter + 1)
        else {
          (appendDigit(acc, currentDigit, counter), digit, 1)
        }
      }

    appendDigit(acc, digit, counter).toString
  }

}
