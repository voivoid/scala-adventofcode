package adventOfCode.problems
package year2017

object problem15 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve1(input, 40000000)
  }

  def solve1(input: Input, pairsToCheck: Int): Int = {
    solve(input, pairsToCheck, calcNext1(_, Mul1), calcNext1(_, Mul2))
  }

  override def solve2(input: Input): Int = {
    solve2(input, 5000000)
  }

  def solve2(input: Input, pairsToCheck: Int): Int = {
    solve(input, pairsToCheck, calcNext2(_, Mul1, 4), calcNext2(_, Mul2, 8))
  }

  private def Mul1 = 16807L
  private def Mul2 = 48271L

  private def solve(input: Input, pairsToCheck: Int, nextA: Long => Long, nextB: Long => Long): Int = {
    val initialPair = parseInput(input)

    Iterator
      .iterate(initialPair) {
        case (a, b) => {
          (nextA(a), nextB(b))
        }
      }
      .take(pairsToCheck)
      .count(hasEqual16Bits)
  }

  private def parseInput(input: Input): (Long, Long) = {
    val Array(a, b) = input.mkString
      .map {
        case c if c.isDigit => c
        case _              => ' '
      }
      .split(' ')
      .filter(_.nonEmpty)
      .map(_.toLong)

    (a, b)
  }

  private def calcNext1(n: Long, prod: Long): Long = {
    n * prod % 2147483647
  }

  private def calcNext2(n: Long, prod: Long, mod: Long): Long = {
    val x = calcNext1(n, prod)
    if (x % mod == 0) x else calcNext2(x, prod, mod)
  }

  private def hasEqual16Bits(pair: (Long, Long)): Boolean = {
    val (a, b) = pair
    val mask = 65535

    (a & mask) == (b & mask)
  }

}
