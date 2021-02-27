package adventOfCode.problems
package year2019

object problem03 extends baseProblem {

  import adventOfCode.utils.path._

  override def solve1(input: Input): Int = {
    val (visitedLocsIter1, visitedLocsIter2) = getVisitedLocs(input)

    val intersections = visitedLocsIter1.toSet.intersect(visitedLocsIter2.toSet) - centralPort

    val distancesToCentralPort = intersections.map(distance(_, centralPort))
    distancesToCentralPort.min
  }

  override def solve2(input: Input): Int = {
    val (visitedLocsIter1, visitedLocsIter2) = getVisitedLocs(input)

    val visitedLocsToDistances1 = visitedLocsIter1.zipWithIndex.toMap
    val visitedLocsToDistances2 = visitedLocsIter2.zipWithIndex.toMap

    val intersections = visitedLocsToDistances1.keys.toSet.intersect(visitedLocsToDistances2.keys.toSet) - centralPort

    val distancesToIntersections = intersections.map(intersection => {
      visitedLocsToDistances1(intersection) + visitedLocsToDistances2(intersection)
    })

    distancesToIntersections.min
  }

  private def getVisitedLocs(input: Input): (Iterator[Location], Iterator[Location]) = {
    val lines = input.getLines()
    val path1 = new DirPath(lines.next(), centralPort)
    val path2 = new DirPath(lines.next(), centralPort)
    (path1.stepByStepLocations(), path2.stepByStepLocations())
  }

  private def centralPort = Location(0, 0)

}
