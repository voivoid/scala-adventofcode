package adventOfCode.problems
package year2015

object problem05 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, isNice1)
  }

  override def solve2(input: Input): Int = {
    solve(input, isNice2)
  }

  private def solve(input: Input, isNice: List[Char] => Boolean): Int = {
    input.getLines().count(str => isNice(str.toList))
  }

  private def isNice1(str: List[Char]): Boolean = {
    hasThreeVowels(str) && hasTwiceInARowLetter(str) && hasNoForbiddenStrings(str)
  }

  private def isNice2(str: List[Char]): Boolean = {
    hasTwoLettersPairWithoutOverlapping(str) && hasTwoLettersWithOneInBetween(str)
  }

  private val vowels = "aeiou"
  private val forbiddenPairs = List(('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y'))

  import adventOfCode.utils.algorithms.{IteratorSlidingTuple, IterableSlidingTuple}

  private def hasThreeVowels(str: List[Char]): Boolean = str.count(c => vowels.contains(c)) >= 3

  private def hasTwiceInARowLetter(str: List[Char]): Boolean = {
    str.iterator.sliding2.exists { case (a, b) => a == b }
  }

  private def hasNoForbiddenStrings(str: List[Char]): Boolean = {
    str.iterator.sliding2.forall(!forbiddenPairs.contains(_))
  }

  private def hasTwoLettersPairWithoutOverlapping(str: List[Char]): Boolean = {
    def filterTriples(s: List[Char]): List[Char] = s match {
      case a :: b :: c :: rest if a == b && b == c => a :: b :: filterTriples(rest)
      case a :: rest                               => a :: filterTriples(rest)
      case Nil                                     => Nil
    }

    filterTriples(str).sliding2.groupBy(identity).exists(_._2.size >= 2)
  }

  private def hasTwoLettersWithOneInBetween(str: List[Char]): Boolean = str match {
    case a :: _ :: c :: _ if a == c => true
    case _ :: rest                  => hasTwoLettersWithOneInBetween(rest)
    case Nil                        => false
  }

}
