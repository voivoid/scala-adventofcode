package adventOfCode.problems
package year2016

object problem01 extends baseProblem {

  import adventOfCode.utils.path._

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.IteratorLast

    val path = makePath(input)
    val finish = path.locations().last

    manhattanDistance(startLocation, finish)
  }

  override def solve2(input: Input): Int = {
    import adventOfCode.utils.algorithms.IteratorFindFirstDuplicate

    val path = makePath(input)
    val firstIntersection = path.stepByStepLocations().findFirstDuplicate.getOrElse(sys.error("Solution not found"))

    manhattanDistance(startLocation, firstIntersection)
  }

  private def makePath(input: Input): BasePath = {
    new TurnPath(input.mkString, startLocation, Bearing.North)
  }

  private def startLocation = Location(0, 0)
}
