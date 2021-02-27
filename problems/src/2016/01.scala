package adventOfCode.problems
package year2016

object problem01 extends baseProblem {

  import adventOfCode.utils.path._

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.IteratorLast

    val path = makePath(input)
    val finish = path.locations().last

    distance(startLocation, finish)
  }

  override def solve2(input: Input): Int = {
    val path = makePath(input)
    val pathHistory = getStepByStepHistory(path)

    val (_, firstIntersection) = pathHistory
      .find { case (visited, location) =>
        visited(location)
      }
      .getOrElse(sys.error("Solution not found"))

    distance(startLocation, firstIntersection)
  }

  private def makePath(input: Input): BasePath = {
    new TurnPath(input.mkString, startLocation, Bearing.North)
  }

  private def getStepByStepHistory(path: BasePath): Iterator[(Set[Location], Location)] = {
    path.stepByStepLocations().drop(1).scanLeft((Set.empty[Location], path.startLocation)) {
      case ((visited, prevLocation), location) => {
        (visited + prevLocation, location)
      }
    }
  }

  private def startLocation = Location(0, 0)
}
