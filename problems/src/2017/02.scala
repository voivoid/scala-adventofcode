package adventOfCode.problems
package year2017

object problem02 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, calcDiff)
  }

  override def solve2(input: Input): Int = {
    solve(input, calcDiv)
  }

  private def calcDiff(ints: Array[Int]): Int = {
    import adventOfCode.utils.algorithms.IteratorMinMax

    val (min, max) = ints.iterator.minmax
    max - min
  }

  private def calcDiv(ints: Array[Int]): Int = {
    val solutions = for {
      a <- ints
      b <- ints
      if a > b && a % b == 0
    } yield a / b

    solutions.headOption.getOrElse(sys.error("no solution"))
  }

  private def solve(input: Input, calc: Array[Int] => Int): Int = {
    input
      .getLines()
      .map(line => {
        val ints = line.split('\t').map(s => s.toInt)
        calc(ints)
      })
      .sum
  }

}
