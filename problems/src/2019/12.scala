package adventOfCode.problems
package year2019

object problem12 extends baseProblem {

  import adventOfCode.utils.geo.Point3D

  override def solve1(input: Input): Int = {
    solve1(input, 1000)
  }

  def solve1(input: Input, steps: Int): Int = {
    val initialMoons = input.getLines().map(parseMoon).toList

    val xPlane = to1D(initialMoons, _.x)
    val yPlane = to1D(initialMoons, _.y)
    val zPlane = to1D(initialMoons, _.z)

    val finalXPlane = LazyList.iterate(xPlane)(runStep)(steps)
    val finalYPlane = LazyList.iterate(yPlane)(runStep)(steps)
    val finalZPlane = LazyList.iterate(zPlane)(runStep)(steps)

    val xyzPlane = finalXPlane.zip(finalYPlane).zip(finalZPlane)
    xyzPlane.map { case ((Moon1D(p1, v1), Moon1D(p2, v2)), Moon1D(p3, v3)) =>
      (p1.abs + p2.abs + p3.abs) * (v1.abs + v2.abs + v3.abs)
    }.sum
  }

  override def solve2(input: Input): Long = {
    val initialMoons = input.getLines().map(parseMoon).toList

    val dx = findCycle(to1D(initialMoons, _.x))
    val dy = findCycle(to1D(initialMoons, _.y))
    val dz = findCycle(to1D(initialMoons, _.z))

    lcm3(dx, dy, dz)
  }

  private type Point = Point3D[Int]
  private type Vector = Point3D[Int]
  private type Moons = List[Moon]
  private type Moons1D = List[Moon1D]

  private[problems] case class Moon(pos: Point, vel: Vector)
  private case class Moon1D(pos: Int, vel: Int)

  private def calcGravityInc(m1: Moon1D, m2: Moon1D): Int = -(m1.pos.compare(m2.pos))

  private def to1D(moons: Moons, f: Point => Int): Moons1D = {
    moons.map(moon => Moon1D(f(moon.pos), f(moon.vel)))
  }

  private def findCycle(moons: Moons1D): Long = {
    import adventOfCode.utils.algorithms.IteratorFindFirstDuplicate
    Iterator.iterate(moons)(runStep).findFirstDuplicateIndex.get.toLong
  }

  private def runStep(moons: Moons1D): Moons1D = {
    getMoonPairs(moons).map {
      case (moon, rest) => {
        val velUpdated = rest.foldLeft(moon) { case (mAcc, m2) =>
          mAcc.copy(vel = mAcc.vel + calcGravityInc(moon, m2))
        }
        val posUpdated = velUpdated.copy(pos = velUpdated.pos + velUpdated.vel)

        posUpdated
      }
    }
  }

  private[problems] def parseMoon(s: String): Moon = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def parser[_: P] = P("<" ~ "x=" ~ num ~ "," ~ "y=" ~ num ~ "," ~ "z=" ~ num ~ ">")

    val (x, y, z) = parseValue(s, parser(_))

    Moon(Point3D(x, y, z), Point3D(0, 0, 0))
  }

  private def getMoonPairs[A](moons: List[A]): List[(A, List[A])] = {
    def getPairs(moons: List[A], rest: List[A]): List[(A, List[A])] = moons match {
      case Nil => Nil
      case m :: ms => {
        (m, rest ::: ms) :: getPairs(ms, m :: rest)
      }
    }

    getPairs(moons, List.empty)
  }

  private def lcm3(a: Long, b: Long, c: Long): Long = {
    import adventOfCode.utils.algorithms.lcm
    lcm(lcm(a, b), c)
  }

}
