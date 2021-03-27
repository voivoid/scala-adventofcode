package adventOfCode.problems
package year2017

object problem09 extends baseProblem {

  override def solve1(input: Input): Int = {
    calcScore(input.toList)._1
  }

  override def solve2(input: Input): Int = {
    calcScore(input.toList)._2
  }

  private case class State(chars: List[Char], scoreAcc: Int, scorePerGroup: Int, isGarbage: Boolean, garbageChars: Int)

  private def calcScore(chars: List[Char]): (Int, Int) = {

    @scala.annotation.tailrec
    def calc(s: State): (Int, Int) = (s.chars, s.isGarbage) match {
      case (Nil, _)               => (s.scoreAcc, s.garbageChars)

      case ('!' :: _ :: xs, true) => calc(s.copy(chars = xs))
      case ('<' :: xs, false)     => calc(s.copy(chars = xs, isGarbage = true))
      case ('>' :: xs, true)      => calc(s.copy(chars = xs, isGarbage = false))
      case (_ :: xs, true)        => calc(s.copy(chars = xs, garbageChars = s.garbageChars + 1))

      case ('{' :: xs, false)     => calc(s.copy(chars = xs, scoreAcc = s.scoreAcc + s.scorePerGroup, scorePerGroup = s.scorePerGroup + 1))
      case ('}' :: xs, false)     => calc(s.copy(chars = xs, scorePerGroup = s.scorePerGroup - 1))

      case (_ :: xs, _)           => calc(s.copy(chars = xs))
    }

    calc(State(chars, 0, 1, false, 0))
  }

}
