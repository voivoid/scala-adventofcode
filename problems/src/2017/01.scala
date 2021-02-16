package adventOfCode.problems
package year2017

object problem01 extends baseProblem {

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.SlidingTuple

    val (inputLen, cycledStr) = readInput(input)

    val pairs =
      cycledStr.take(inputLen + 1).sliding2

    sumPairs(pairs.map { case (a, b) =>
      (a, b)
    })
  }

  override def solve2(input: Input): Int = {
    val (inputLen, cycledStr) = readInput(input)
    val (iter1, iter2) = cycledStr.duplicate

    val pairs = iter1
      .take(inputLen)
      .zip(iter2.drop(inputLen / 2))
    sumPairs(pairs)
  }

  private def sumPairs(pairs: Iterator[(Char, Char)]): Int = {
    pairs.map { case (a, b) =>
      if (a == b) a.asDigit else 0
    }.sum
  }

  private def readInput(input: Input): (Int, Iterator[Char]) = {
    val inputStr = input.mkString
    val cycledStr =
      inputStr.iterator.concat(inputStr.iterator)
    (inputStr.size, cycledStr)
  }

}
