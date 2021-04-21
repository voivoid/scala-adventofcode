package adventOfCode.problems
package year2017

object problem03 extends baseProblem {

  import adventOfCode.utils.geo.Point

  override def solve1(input: Input): Int = {
    stepsToCenter(getInputN(input))
  }

  override def solve2(input: Input): Int = {
    val inputN = getInputN(input)
    getSpiralValues().find(_ > inputN).getOrElse(sys.error("No solution"))
  }

  private def stepsToCenter(n: Int): Int = {
    require(n > 0)

    if (n == 1) 0
    else {
      def square(n: Int): Int = n * n

      val ring = math.ceil((math.sqrt(n.toDouble) - 1.0) / 2.0).toInt
      val ringsFirstNum = square((ring - 1) * 2 + 1) + 1
      val ringQuarterIndex = (n - ringsFirstNum) % (ring * 2)
      val steps = (ringQuarterIndex - (ring - 1)).abs + ring

      steps
    }
  }

  private type Location = Point[Int]
  private type ValuesMap = Map[Location, Int]

  private def getSpiralValues(): Iterator[Int] = {
    val startLocation = Point(0, 0)
    val spiralPath = makeSpiralPath(startLocation)

    spiralPath
      .scanLeft((Map(startLocation -> 1), 1)) {
        case ((valuesMap, _), location) => {
          val neighboursSum = getNeighbourValues(location, valuesMap)
          (valuesMap + (location -> neighboursSum), neighboursSum)
        }
      }
      .map { case (_, value) => value }
  }

  private def getNeighbourValues(location: Location, valuesMap: ValuesMap): Int = {
    adventOfCode.utils.path.neighbours8(location).map(valuesMap.getOrElse(_, 0)).sum
  }

  private def up = Point(0, 1)
  private def right = Point(1, 0)
  private def down = Point(0, -1)
  private def left = Point(-1, 0)

  private def makeSpiralPath(startLocation: Location): Iterator[Location] = {
    import adventOfCode.utils.algorithms.IterableCycle

    val moves = List(right, up, left, down).cycled
    val steps = Iterator.from(1).flatMap(n => Iterator(n, n))

    val spiralMoves = moves.zip(steps).flatMap { case (move, steps) =>
      Iterator.fill(steps)(move)
    }

    spiralMoves
      .scanLeft(startLocation) { case (Point(currentX, currentY), Point(dx, dy)) =>
        Point(currentX + dx, currentY + dy)
      }
      .drop(1)
  }

  private def getInputN(input: Input): Int = input.mkString.trim.toInt

}
