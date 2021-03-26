package adventOfCode.problems
package year2015

object problem08 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, calcLiteralAndMemoryStrSizeDiff)
  }

  override def solve2(input: Input): Int = {
    solve(input, calcEncodedAndOriginalSizeDiff)
  }

  private def solve(input: Input, calc: String => Int): Int = {
    input.getLines().map(calc).sum
  }

  private def calcLiteralAndMemoryStrSizeDiff(str: String): Int = {

    @scala.annotation.tailrec
    def calcCharsInMemory(chars: List[Char], acc: Int): Int = chars match {
      case Nil                           => acc
      case '"' :: rest                   => calcCharsInMemory(rest, acc)
      case '\\' :: '\\' :: rest          => calcCharsInMemory(rest, acc + 1)
      case '\\' :: '"' :: rest           => calcCharsInMemory(rest, acc + 1)
      case '\\' :: 'x' :: _ :: _ :: rest => calcCharsInMemory(rest, acc + 1)

      case _ :: rest => calcCharsInMemory(rest, acc + 1)
    }

    val inMemory = calcCharsInMemory(str.toList, 0)
    str.size - inMemory
  }

  private def calcEncodedAndOriginalSizeDiff(str: String): Int = {

    @scala.annotation.tailrec
    def calcEncodedSize(chars: List[Char], acc: Int): Int = chars match {
      case Nil          => acc
      case '"' :: rest  => calcEncodedSize(rest, acc + 2)
      case '\\' :: rest => calcEncodedSize(rest, acc + 2)
      case _ :: rest    => calcEncodedSize(rest, acc + 1)
    }

    val encodedSize = 2 + calcEncodedSize(str.toList, 0)
    encodedSize - str.size
  }

}
