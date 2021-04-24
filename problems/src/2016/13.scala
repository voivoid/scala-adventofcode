package adventOfCode.problems
package year2016

object problem13 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.neighbours4

  override def solve1(input: Input): Int = {
    solve1(input, Point(31, 39))
  }

  def solve1(input: Input, finalPos: Coord): Int = {
    val office = input.mkString.toInt
    val startPos = Point(1, 1)

    findRoute(Set(startPos), Set.empty, finalPos, isOpen(_, office), 0).getOrElse(sys.error("no solution"))
  }

  override def solve2(input: Input): Int = {
    val office = input.mkString.toInt
    val startPos = Point(1, 1)

    countReachableLocations(Set(startPos), Set.empty, maxDistance = 50, isOpen(_, office), 0)
  }

  private type Coord = Point[Int]

  private[problems] def isWall(coord: Coord, office: Int): Boolean = !isOpen(coord, office)
  private def isOpen(coord: Coord, office: Int): Boolean = {
    val Point(x, y) = coord
    val s = x * x + 3 * x + 2 * x * y + y + y * y + office
    val oneBits = calcOneBits(s)

    oneBits % 2 == 0
  }

  private def calcOneBits(n: Int): Int = {

    @scala.annotation.tailrec
    def calc(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else {
        import scala.math.Integral.Implicits._

        val (div, rem) = n /% 2
        calc(div, acc + rem)
      }
    }

    calc(n, 0)
  }

  @scala.annotation.tailrec
  private def countReachableLocations(
    coordsToCheck: Set[Coord],
    visited: Set[Coord],
    maxDistance: Int,
    canStep: Coord => Boolean,
    steps: Int
  ): Int = {
    if (coordsToCheck.isEmpty || steps == maxDistance + 1) visited.size
    else {
      val nextVisited = coordsToCheck ++ visited
      val nextCoordsToCheck = coordsToCheck.flatMap(nextSteps(_, canStep)) -- nextVisited

      countReachableLocations(nextCoordsToCheck, nextVisited, maxDistance, canStep, steps + 1)
    }
  }

  @scala.annotation.tailrec
  private def findRoute(
    coordsToCheck: Set[Coord],
    visited: Set[Coord],
    finalPos: Coord,
    canStep: Coord => Boolean,
    steps: Int
  ): Option[Int] = {
    if (coordsToCheck.isEmpty) None
    else if (coordsToCheck.contains(finalPos)) Some(steps)
    else {
      val nextVisited = coordsToCheck ++ visited
      val nextCoordsToCheck = coordsToCheck.flatMap(nextSteps(_, canStep)) -- nextVisited

      findRoute(nextCoordsToCheck, nextVisited, finalPos, canStep, steps + 1)
    }
  }

  private def nextSteps(coord: Coord, canStep: Coord => Boolean): List[Coord] = {
    val neighbours =
      if (coord.x >= 1 && coord.y >= 1) neighbours4(coord) else neighbours4(coord).filter(c => c.x >= 0 && c.y >= 0)
    neighbours.filter(canStep)
  }

}
