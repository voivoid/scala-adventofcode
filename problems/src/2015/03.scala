package adventOfCode.problems
package year2015

object problem03 extends baseProblem {

  override def solve1(input: Input): Int = {
    getVisitedLocations(input.iterator).size
  }

  override def solve2(input: Input): Int = {
    val (santaIter, robotIter) = input.iterator.zipWithIndex.partition { case (_, i) => i % 2 == 0 }

    val santaLocations = getVisitedLocations(santaIter.map(_._1))
    val robotLocations = getVisitedLocations(robotIter.map(_._1))

    santaLocations.union(robotLocations).size
  }

  private type Location = (Int, Int)

  private def getMovementMap = Map[Char, Location]('>' -> ((1, 0)), '<' -> ((-1, 0)), '^' -> ((0, -1)), 'v' -> ((0, 1)))

  private def getVisitedLocations(i: Iterator[Char]): Set[Location] = {
    import cats.implicits.catsSyntaxSemigroup

    val movementMap = getMovementMap
    val visitedLocatios = i.filter(!_.isWhitespace).scanLeft((0, 0)) { case (loc, move) =>
      loc |+| movementMap(move)
    }

    visitedLocatios.toSet
  }

}
