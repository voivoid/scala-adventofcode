package adventOfCode.problems
package year2017

object problem20 extends baseProblem {

  import adventOfCode.utils.geo.Point3D

  override def solve1(input: Input): Int = {
    val particles = input.getLines().map(parseParticle).toList

    val minAcc = particles.iterator.map(particle => sumAbsVals(particle.acc)).min
    val psWithMinAcc = particles.iterator.zipWithIndex.filter { case (particle, _) => minAcc == sumAbsVals(particle.acc) }.toList

    val psWithSameSignPosVelAcc = Iterator
      .iterate(psWithMinAcc)(ps => ps.map { case (particle, i) => (calcNextState(particle), i) })
      .find(ps => ps.forall { case (particle, _) => hasSameSigns(particle) })
      .get

    val velPosSumTuples = psWithSameSignPosVelAcc.map { case (particle, i) => ((sumAbsVals(particle.vel), sumAbsVals(particle.pos)), i) }

    val (_, closestParticleIndex) = velPosSumTuples.minBy(_._1)
    closestParticleIndex
  }

  override def solve2(input: Input): Int = {
    val particles = input.getLines().map(parseParticle).toList

    import adventOfCode.utils.algorithms.IteratorLast
    val iterations = 1000 // TODO: find a reliable stop condition instead of a magic iterations number
    val ps = Iterator.iterate(particles, iterations)(ps => eliminateCollisions(runTick(ps))).last

    ps.size
  }

  private def sumAbsVals(c: Coord): Long = c.x.abs + c.y.abs + c.z.abs

  private type Coord = Point3D[Long]
  private type Vector = Point3D[Long]
  private case class Particle(pos: Coord, vel: Vector, acc: Vector)
  private type Particles = List[Particle]

  private def runTick(particles: Particles): Particles = {
    particles.map(calcNextState)
  }

  private def eliminateCollisions(particles: Particles): Particles = {
    particles.groupBy(_.pos).withFilter(_._2.size == 1).map(_._2.head).toList
  }

  private def calcNextState(particle: Particle): Particle = {
    val Particle(Point3D(x, y, z), Point3D(vx, vy, vz), a @ Point3D(ax, ay, az)) = particle

    val nextV = Point3D(vx + ax, vy + ay, vz + az)
    val nextP = Point3D(x + nextV.x, y + nextV.y, z + nextV.z)

    Particle(nextP, nextV, a)
  }

  private def hasSameSigns(particle: Particle): Boolean = {
    import particle._
    hasSameSign(pos.x, vel.x, acc.x) && hasSameSign(pos.y, vel.y, acc.y) && hasSameSign(pos.z, vel.z, acc.z)
  }
  private def hasSameSign(p: Long, v: Long, a: Long): Boolean = (p >= 0 && v >= 0 && a >= 0) || (p <= 0 && v <= 0 && a <= 0)

  private def parseParticle(s: String): Particle = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, numL}

    def coord[_: P] = P("<" ~ numL ~ "," ~ numL ~ "," ~ numL ~ ">").map { case (x, y, z) => Point3D(x, y, z) }
    def p[_: P] = P("p=" ~ coord)
    def v[_: P] = P("v=" ~ coord)
    def a[_: P] = P("a=" ~ coord)

    def parser[_: P] = P(p ~ "," ~ v ~ "," ~ a).map(Particle tupled _)

    parseValue(s, parser(_))
  }

}
