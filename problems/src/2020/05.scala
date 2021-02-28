package adventOfCode.problems
package year2020

object problem05 extends baseProblem {

  override def solve1(input: Input): Int = {
    input.getLines().map(calcSeatId).max
  }

  override def solve2(input: Input): Int = {
    import adventOfCode.utils.algorithms.IterableMinMax

    val seats = input.getLines().map(calcSeatId).toList
    val (min, max) = seats.minmax

    (min to max).sum - seats.sum
  }

  private def calcSeatId(pass: String): Int = {
    assert(pass.size == 10)

    val (rowStr, colStr) = pass.splitAt(7)
    calcRow(rowStr) * 8 + calcCol(colStr)
  }

  private def calcRow(rowStr: String): Int = calcSeat(rowStr, LowerHalf = 'F', UpperHalf = 'B')
  private def calcCol(colStr: String): Int = calcSeat(colStr, LowerHalf = 'L', UpperHalf = 'R')

  private def calcSeat(letters: String, LowerHalf: Char, UpperHalf: Char) = {
    val startSeat = 0
    val endSeat = math.pow(2.0, letters.size.toDouble).toInt - 1

    val (s1, s2) = letters.foldLeft((startSeat, endSeat)) {
      case ((from, to), letter) => {
        val mid = (from + to + 1) / 2
        letter match {
          case LowerHalf => (from, mid - 1)
          case UpperHalf => (mid, to)
        }
      }
    }

    assert(s1 == s2)
    s1
  }

}
