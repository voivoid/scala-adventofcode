package adventOfCode.problems
package year2016

object problem06 extends baseProblem {

  override def solve1(input: Input): String = {
    solve(input, findMostFrequentChar)
  }

  override def solve2(input: Input): String = {
    solve(input, findMostFrequentChar(_)(Ordering[Int].reverse))
  }

  private def solve(input: Input, recoverChar: Vector[Char] => Char): String = {
    val colums = input.getLines().toVector.transpose
    colums.map(recoverChar).mkString

  }

  private def findMostFrequentChar(chars: Vector[Char])(implicit ordering: Ordering[Int]): Char = {
    chars.groupBy(identity).maxBy { case (_, charsGroup) => charsGroup.size }(ordering)._1
  }

}
