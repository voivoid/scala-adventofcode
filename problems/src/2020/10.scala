package adventOfCode.problems
package year2020

object problem10 extends baseProblem {

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.IterableSlidingTuple

    val joltDiffs = sortedAdapters(input).sliding2.map { case (j1, j2) => j2 - j1 }

    val j1Diff = joltDiffs.count(_ == 1)
    val j2Diff = joltDiffs.count(_ == 3) + 1

    j1Diff * j2Diff
  }

  override def solve2(input: Input): Long = {
    val jolts = sortedAdapters(input)
    calcConnections(jolts.toList, Map.empty)._1
  }

  private def sortedAdapters(input: Input): Vector[Int] = {
    0 +: input.getLines().map(_.toInt).toVector.sorted
  }

  private type ConnectionsCache = Map[Int, Long]

  private def calcConnections(adapters: List[Int], cache: ConnectionsCache): (Long, ConnectionsCache) = adapters match {
    case Nil      => (1, cache)
    case _ :: Nil => (1, cache)
    case j :: js => {
      val compatibleJs = js.tails.takeWhile(_.headOption.exists(_ <= j + 3))
      val (connections, updatedCache) = compatibleJs.foldLeft((0L, cache)) {
        case ((connsAcc, cacheAcc), tail) => {
          val cachedConns = cacheAcc.get(tail.head)
          val (tailConns, updatedCache) = cachedConns.map((_, cacheAcc)).getOrElse(calcConnections(tail, cacheAcc))
          (connsAcc + tailConns, updatedCache)
        }
      }

      (connections, updatedCache + (j -> connections))
    }
  }

}
