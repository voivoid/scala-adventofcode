package adventOfCode.problems
package year2015

object problem09 extends baseProblem {

  override def solve1(input: Input): Int = {
    val distances = solve(input)
    distances.min
  }

  override def solve2(input: Input): Int = {
    val distances = solve(input)
    distances.max
  }

  private def solve(input: Input): Iterator[Distance] = {
    val routes = input.getLines().map(parseRoute)
    val distanceMap = makeDistanceMap(routes)

    val cities = distanceMap.iterator.map { case ((city, _), _) => city }.toSet.toVector

    val distancesIter = cities.permutations.map(permutation => {
      permutation
        .sliding(2)
        .map(citiesPair =>
          (citiesPair: @unchecked) match {
            case Vector(from, to) => distanceMap((from, to))
          }
        )
        .sum
    })

    distancesIter
  }

  private type City = String
  private type Distance = Int
  private case class Route(from: City, to: City, distance: Distance)

  private def makeDistanceMap(routes: Iterator[Route]): Map[(City, City), Distance] = {
    routes.flatMap { case Route(from, to, distance) =>
      List((from, to) -> distance, (to, from) -> distance)
    }.toMap
  }

  private def parseRoute(str: String): Route = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, alpha, num}

    def city[_: P] = P(alpha.repX(1).!)
    def parser[_: P] = P(city ~ "to" ~ city ~ "=" ~ num).map(Route tupled _)

    parseValue(str, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

    assertMatch(parseRoute("London to Dublin = 464")) { case Route("London", "Dublin", 464) =>
    }
  }

}
