package adventOfCode.problems
package year2019

object problem10 extends baseProblem {

  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.algorithms.{gcd, IteratorNth}

  override def solve1(input: Input): Int = {
    val asteroids = parseAsteroids(input)
    asteroids.map(countVisibleAsteroids(_, asteroids)).max
  }

  override def solve2(input: Input): Int = {
    solve2(input, lastAsteroid = 200)
  }

  def solve2(input: Input, lastAsteroid: Int): Int = {
    val asteroids = parseAsteroids(input)
    val centerCoord = asteroids.maxBy(countVisibleAsteroids(_, asteroids))

    // sorted clockwise ( from 90° to -270° )
    val asteroidsSortedByAngle = groupAsteroidsByAngle(centerCoord, asteroids).sortBy(_.angle)(Ordering[Double].reverse)

    val asteroidsOrderedByDestruction = Iterator
      .from(0)
      .flatMap(fullRotationIndex => asteroidsSortedByAngle.map(_.asteroids.lift(fullRotationIndex)))
      .flatten

    val finalAsteroid = asteroidsOrderedByDestruction.nth(lastAsteroid) + centerCoord
    finalAsteroid.x * 100 + finalAsteroid.y
  }

  private type Coord = Point[Int]

  private def parseAsteroids(input: Input): List[Coord] = {
    input
      .getLines()
      .zipWithIndex
      .flatMap { case (line, y) =>
        line.zipWithIndex.collect { case ('#', x) => Point(x, y) }
      }
      .toList
  }

  private def countVisibleAsteroids(theAsteroid: Coord, asteroids: Seq[Coord]): Int = {
    val shiftedAsteroids = asteroids.map(_ - theAsteroid)

    val visibleAsteroids = shiftedAsteroids.foldLeft(Set.empty[Coord]) {
      case (visibleAsteroids, Point(0, 0)) => visibleAsteroids
      case (visibleAsteroids, asteroid)    => visibleAsteroids.incl(closestVisibleCoord(asteroid))
    }

    visibleAsteroids.size
  }

  private case class AsteroidsGroupedByAngle(angle: Double, asteroids: Seq[Coord])

  private def groupAsteroidsByAngle(center: Coord, asteroids: Seq[Coord]): Vector[AsteroidsGroupedByAngle] = {
    val shiftedAsteroids = asteroids.withFilter(_ != center).map(_ - center)

    shiftedAsteroids
      .groupBy(closestVisibleCoord)
      .iterator
      .map {
        case (asteroid, sameAngleAsteroids) => {
          // negate y so upper asteroids will have positive angle
          val angle = math.toDegrees(math.atan2(-asteroid.y.toDouble, asteroid.x.toDouble))

          // map qudrant II angles ( 90°..180°  to -270°..-180°)
          // to simplify clockwise sort by angle value ( 90° .. -270° )
          val angle2 = if (angle > 90.0 && angle <= 180.0) angle - 360.0 else angle

          AsteroidsGroupedByAngle(angle2, sortAsteroidsByDistance(sameAngleAsteroids))
        }
      }
      .toVector
  }

  private def sortAsteroidsByDistance(asteroid: Seq[Coord]): Seq[Coord] = {
    asteroid.sortBy(p => p.x.abs + p.y.abs)
  }

  private def closestVisibleCoord(asteroid: Coord): Coord = {
    val gcdValue = gcd(asteroid.x.abs, asteroid.y.abs)
    Point(asteroid.x / gcdValue, asteroid.y / gcdValue)
  }

}
