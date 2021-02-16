package adventOfCode.problems
package year2020

object problem01 extends baseProblem {

  override def solve1(input: Input): Int = {
    val entries =
      input.getLines().map(_.toInt).toSet
    val (n1, n2) =
      tryFindSumOf(2020, entries).getOrElse(sys.error("no solution"))

    n1 * n2
  }

  override def solve2(input: Input): Int = {
    val entries =
      input.getLines().map(_.toInt).toSet

    val solutions = entries.view.flatMap { entry =>
      tryFindSumOf(2020 - entry, entries - entry).map { case (n1, n2) =>
        (entry, n1, n2)
      }
    }

    val (n1, n2, n3) =
      solutions.headOption.getOrElse(sys.error("no solution"))

    n1 * n2 * n3
  }

  private type Entry = Int

  private def tryFindSumOf(sum: Entry, entries: Set[Entry]): Option[(Entry, Entry)] = {
    entries
      .find(entry => entries(sum - entry))
      .map(n => (n, sum - n))
  }

}
