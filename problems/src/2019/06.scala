package adventOfCode.problems
package year2019

object problem06 extends baseProblem {

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.IteratorToMultimap
    val planet2orbit = input
      .getLines()
      .map(line => {
        val Array(planet, orbit) = line.split(')')
        (planet, orbit)
      })

    calcOrbits(planet2orbit.toMultimap)
  }

  override def solve2(input: Input): Int = {
    val orbitsMap = input
      .getLines()
      .map(line => {
        val Array(planet, orbit) = line.split(')')
        (orbit, planet)
      })
      .toMap

    val youPath = findPath("YOU", "COM", orbitsMap)
    val santaPath = findPath("SAN", "COM", orbitsMap)
    val commonPathLen = findCommonPathLen(youPath, santaPath) + 1

    youPath.size + santaPath.size - 2 * commonPathLen
  }

  private type Orbit = String
  private type Planet = String
  private type PlanetsMultiMap = Map[Planet, Set[Orbit]]
  private type OrbitsMap = Map[Orbit, Planet]
  private type Path = Vector[Planet]

  private def calcOrbits(planetsMultimap: PlanetsMultiMap): Int = {

    @scala.annotation.tailrec
    def calc(planets: Set[Planet], totalOrbits: Int, distance: Int): Int = {
      if (planets.isEmpty) totalOrbits
      else {
        val orbits = planets.size * distance
        val nextPlanets = planets.map(planet => planetsMultimap.getOrElse(planet, Set.empty)).flatten

        calc(nextPlanets, totalOrbits + orbits, distance + 1)
      }
    }

    calc(Set("COM"), 0, 0)
  }

  private def findPath(initialFrom: Orbit, to: Planet, orbitsMap: OrbitsMap): Path = {
    Vector.unfold(initialFrom)(from =>
      if (from == to) None
      else Some((from, orbitsMap(from)))
    )
  }

  private def findCommonPathLen(path1: Path, path2: Path): Int = {
    path1.reverseIterator.zip(path2.reverseIterator).indexWhere { case (p1, p2) => p1 != p2 }
  }

}
