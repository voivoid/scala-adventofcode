package adventOfCode.problems
package year2016

object problem03 extends baseProblem {

  override def solve1(input: Input): Int = {
    input.getLines().map(parseTriangleSides).count(isValidTriangle)
  }

  override def solve2(input: Input): Int = {
    val triples = input.getLines().map(parseTriangleSides).grouped(3)

    triples
      .flatMap(triple => {
        triple.transpose
      })
      .count(isValidTriangle)
  }

  private def parseTriangleSides(line: String): Seq[Int] = {
    import fastparse._
    import SingleLineWhitespace._
    import adventOfCode.utils.parse.{num, parseValue}

    def parser[_: P] = P(Pass ~ num ~ num ~ num)

    val (a, b, c) = parseValue(line, parser(_))
    Seq(a, b, c)
  }

  private def isValidTriangle(sides: Seq[Int]): Boolean = {
    val Seq(a, b, c) = sides.sorted: @unchecked
    a + b > c
  }

}
