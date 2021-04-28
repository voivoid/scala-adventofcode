package adventOfCode.problems
package year2015

object problem14 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve1(input, defaultRaceTime)
  }

  def solve1(input: Input, raceTime: Int): Int = {
    val deers = input.getLines().map(parseDeer)
    val flyDistances = deers.map(calcFlyDistance(_, raceTime))

    flyDistances.max
  }

  override def solve2(input: Input): Int = {
    solve2(input, defaultRaceTime)
  }

  def solve2(input: Input, raceTime: Int): Int = {
    val deers = input.getLines().map(parseDeer)
    val deerDistances = deers.map(calcDistancesEachSecond(_, raceTime)).toList

    val deerScoresEachSecond = deerDistances.transpose.map { distances =>
      val maxDistance = distances.max
      val scores = distances.map(d => if (d == maxDistance) 1 else 0)
      scores
    }.transpose

    val totalScores = deerScoresEachSecond.map(_.sum)
    totalScores.max
  }

  private def defaultRaceTime = 2503

  private def calcDistancesEachSecond(deer: Deer, raceTime: Int): List[Int] = {
    val seconds = 1 to raceTime
    seconds.iterator.map(calcFlyDistance(deer, _)).toList
  }

  private def calcFlyDistance(deer: Deer, raceTime: Int): Int = {
    import scala.math.Integral.Implicits._

    val (fullCycles, halfCycleSeconds) = raceTime /% (deer.flySeconds + deer.restSeconds)

    val halfCycleDistance = halfCycleSeconds.min(deer.flySeconds) * deer.speed
    val distance = fullCycles * (deer.flySeconds * deer.speed) + halfCycleDistance

    distance
  }

  private[problems] case class Deer(name: String, speed: Int, flySeconds: Int, restSeconds: Int)

  private[problems] def parseDeer(s: String): Deer = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, alpha, num}

    def name[_: P] = P(alpha.repX(1).!)
    def parser[_: P] =
      P(name ~ "can" ~ "fly" ~ num ~ "km/s" ~ "for" ~ num ~ "seconds," ~ "but" ~ "then" ~ "must" ~ "rest" ~ "for" ~ num ~ "seconds.")
        .map(Deer tupled _)

    parseValue(s, parser(_))
  }

}
