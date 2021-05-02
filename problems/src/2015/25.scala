package adventOfCode.problems
package year2015

object problem25 extends baseProblem {

  override def solve1(input: Input): Long = {
    val (row, col) = parseInput(input)
    val codeIndex = getCodeIndex(row, col)

    calcCode(codeIndex)
  }

  override def solve2(input: Input): Int = {
    0 // day 25 has no second part
  }

  private def parseInput(input: Input): (Int, Int) = {
    val Array(row, col) = input.mkString
      .map {
        case c if c.isDigit => c
        case _              => ' '
      }
      .split(' ')
      .filter(_.nonEmpty)
      .map(_.toInt)

    (row, col)
  }

  private def calcCode(index: Int): Long = {
    import adventOfCode.utils.algorithms.IteratorLast

    Iterator.iterate(20151125L, index)(calcNextCode).last
  }

  private def calcNextCode(prev: Long): Long = {
    prev * 252533 % 33554393
  }

  private def getCodeIndex(row: Int, col: Int): Int = {
    val diag = row + col - 1
    (1 until diag).sum + col
  }

}
