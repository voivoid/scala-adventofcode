package adventOfCode.problems
package year2015

object problem13 extends baseProblem {

  override def solve1(input: Input): Int = {
    val seatsMap = makeSeatsMap(input.getLines().map(parseSeat))
    val names = getNames(seatsMap)

    calcMaxHappiness(names, seatsMap)
  }

  override def solve2(input: Input): Int = {
    val seatsMap = makeSeatsMap(input.getLines().map(parseSeat))
    val names = getNames(seatsMap)

    val me = "ME!!!"

    val namesAndMe = names.appended(me)
    val seatsMapAndMe = seatsMap ++ names.flatMap(name => List((name, me) -> 0, (me, name) -> 0))

    calcMaxHappiness(namesAndMe, seatsMapAndMe)
  }

  private[problems] case class SeatInfo(name: String, happiness: Happiness, neighbour: String)

  private[problems] def parseSeat(s: String): SeatInfo = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num, alpha}

    def name[_: P] = P(alpha.repX(1)).!
    def happiness[_: P] = P(("gain" ~ num) | ("lose" ~ num).map(v => -v))
    def parser[_: P] = P(name ~ "would" ~ happiness ~ "happiness" ~ "units" ~ "by" ~ "sitting" ~ "next" ~ "to" ~ name ~ ".")
      .map(SeatInfo tupled _)

    parseValue(s, parser(_))
  }

  private type Happiness = Int
  private type SeatsMap = Map[(String, String), Happiness]

  private def makeSeatsMap(seatInfo: Iterator[SeatInfo]): SeatsMap = {
    seatInfo.map { case SeatInfo(left, happiness, right) => (left, right) -> happiness }.toMap
  }

  private def getNames(seatsMap: SeatsMap): Vector[String] = {
    seatsMap.keys.flatMap { case (n1, n2) => List(n1, n2) }.toVector.distinct
  }

  private def calcTotalHappiness(names: Vector[String], seatsMap: SeatsMap): Happiness = {
    val pairs = names.appended(names.head).prepended(names.last).sliding(3).flatMap {
      case Vector(left, center, right) => List((center, left), (center, right))
      case _                           => sys.error("unexpected")
    }

    pairs.map(seatsMap).sum
  }

  private def calcMaxHappiness(names: Vector[String], seatsMap: SeatsMap) = {
    val permutations = names.permutations
    permutations.map(calcTotalHappiness(_, seatsMap)).max
  }

}
