package adventOfCode.problems
package year2018

object problem02 extends baseProblem {

  import adventOfCode.utils.collections.CharTrie

  override def solve1(input: Input): Int = {
    import cats.implicits.catsSyntaxSemigroup

    val (doubles, triples) = input.getLines().map(countDoublesAndTriples).foldLeft((0, 0))(_ |+| _)

    doubles * triples
  }

  override def solve2(input: Input): String = {
    val ids = input.getLines().toArray
    val trie = ids.foldLeft(CharTrie()) { case (trie, id) => trie.insert(id) }

    val (idWithMismatch, mismatchIndex) = ids.flatMap(findSingleMismatch(_, trie)).headOption.getOrElse(sys.error("no solution"))
    val idWithoutMismatch = idWithMismatch.substring(0, mismatchIndex) ++ idWithMismatch.substring(mismatchIndex + 1)

    idWithoutMismatch
  }

  private def countDoublesAndTriples(id: String): (Int, Int) = {
    val groupedBySameChars = id.groupBy(identity)
    val hasDoubles = if (groupedBySameChars.values.exists(_.size == 2)) 1 else 0
    val hasTriples = if (groupedBySameChars.values.exists(_.size == 3)) 1 else 0

    (hasDoubles, hasTriples)
  }

  private def findSingleMismatch(id: String, trie: CharTrie): Option[(String, Int)] = {
    def findImpl(trie: CharTrie, idIndexed: List[(Char, Int)], prevMismatch: Option[Int]): Option[Int] = idIndexed match {
      case (char, charIdx) :: chars => {
        trie.getNodes
          .flatMap(node => {
            val matched = node.value == char

            if (matched) findImpl(node, chars, prevMismatch)
            else if (prevMismatch.isDefined) None
            else findImpl(node, chars, Some(charIdx))
          })
          .headOption
      }

      case Nil => prevMismatch
    }

    findImpl(trie, id.toList.zipWithIndex, None).map(i => (id, i))
  }

}
