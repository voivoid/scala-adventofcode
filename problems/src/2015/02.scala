package adventOfCode.problems
package year2015

object problem02 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, calcMaterial1)
  }

  override def solve2(input: Input): Int = {
    solve(input, calcMaterial2)
  }

  private def solve(input: Input, calcMaterial: Dims => Int): Int = {
    input.getLines().iterator.map(calcMaterial compose sortDims compose parseDims).sum
  }

  type Dims = (Int, Int, Int)

  private def parseDims(line: String): Dims = {
    import fastparse._
    import SingleLineWhitespace._
    import adventOfCode.utils.parse.{num, parseValue}

    def parser[_: P] = P(num ~ "x" ~ num ~ "x" ~ num)
    parseValue(line.mkString, parser(_))
  }

  private def sortDims(dims: Dims): Dims = {
    val (l, w, h) = dims
    val min = l min w min h
    val max = l max w max h
    val mid = l + w + h - min - max

    (min, mid, max)
  }

  private def calcMaterial1( dims: Dims ) : Int = {
    val (min, mid, max) = dims
    val area = 2 * min * mid + 2 * min * max + 2 * mid * max

    area + min * mid
  }

  private def calcMaterial2( dims: Dims ) : Int = {
    val (min, mid, max) = dims

    min * 2 + mid * 2 + min * mid * max
  }

}
