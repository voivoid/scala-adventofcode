package adventOfCode.problems
package year2017

object problem04 extends baseProblem {

  override def solve1(input: Input): Int = {
    input.getLines().count(isGoodPassphrase(_, checkAnagrams = false))
  }

  override def solve2(input: Input): Int = {
    input.getLines().count(isGoodPassphrase(_, checkAnagrams = true))
  }

  private def isGoodPassphrase(passphrase: String, checkAnagrams: Boolean): Boolean = {
    import adventOfCode.utils.algorithms.{IteratorFindFirstDuplicate, IteratorSplit}

    val words = {
      val words = passphrase.iterator.splitBy(' ')
      if (checkAnagrams) words.map(_.sorted) else words
    }

    val noDuplicates = words.findFirstDuplicate.isEmpty
    noDuplicates
  }

}
