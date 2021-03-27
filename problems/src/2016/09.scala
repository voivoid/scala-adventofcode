package adventOfCode.problems
package year2016

import scala.annotation.unused

object problem09 extends baseProblem {

  override def solve1(input: Input): Long = {
    calcDecompressedLen(input.mkString, calcStrLen)
  }

  override def solve2(input: Input): Long = {
    calcDecompressedLen(input.mkString, calcRecStrLen)
  }

  private def calcStrLen(@unused str: String, subsequent: Int, repeat: Int): Long = {
    (subsequent * repeat).toLong
  }

  private def calcRecStrLen(str: String, @unused subsequent: Int, repeat: Int): Long = {
    calcDecompressedLen(str, calcRecStrLen) * repeat
  }

  private type CalcLenFunc = (String, Int, Int) => Long

  private def calcDecompressedLen(string: String, calcLen: CalcLenFunc): Long = {
    import fastparse._
    import fastparse.NoWhitespace._
    import adventOfCode.utils.parse.{parseValue, num, alpha}

    def chars[_: P]: P[Long] = P(alpha.rep(1)).map(_.size.toLong)
    def marker[_: P] = P("(" ~ num ~ "x" ~ num ~ ")")

    def decompress[_: P]: P[Long] = {
      P(marker.flatMap { case (subsequent, repeat) =>
        AnyChar.rep(exactly = subsequent).!.map(calcLen(_, subsequent, repeat))
      })
    }

    def parser[_: P] = P(chars | decompress).rep.map(_.sum)

    parseValue(string, parser(_))
  }

}
