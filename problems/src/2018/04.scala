package adventOfCode.problems
package year2018

object problem04 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, findMostSleepyGuard = calcTotalMinutesAsleep)
  }

  override def solve2(input: Input): Int = {
    solve(input, findMostSleepyGuard = findMostSleepyMinute(_).sleepFrequency)
  }

  private def solve(input: Input, findMostSleepyGuard: Seq[Shift] => Int): Int = {
    val shiftsMap = makeGuardIdToShiftsMap(input)
    val mostSleepyGuardId = shiftsMap.maxBy { case (_, shifts) => findMostSleepyGuard(shifts) }._1
    val mostSleepyMinute = findMostSleepyMinute(shiftsMap(mostSleepyGuardId)).minute

    mostSleepyGuardId * mostSleepyMinute
  }

  private type SleepInterval = (Int, Int)
  private type GuardId = Int
  private type ShiftsMap = Map[GuardId, Seq[Shift]]
  private case class Shift(guardId: GuardId, sleepIntervals: Seq[SleepInterval])

  private def makeGuardIdToShiftsMap(input: Input): ShiftsMap = {
    val sortedInput = input.getLines().toVector.sorted.mkString
    val shifts = parseShifts(sortedInput)

    shifts.groupBy(_.guardId)
  }

  private case class SleepFrequency(minute: Int, sleepFrequency: Int)
  private def findMostSleepyMinute(shifts: Seq[Shift]): SleepFrequency = {
    val allIntervals = shifts.flatMap(_.sleepIntervals)
    val minutes = 0 until 60

    val sleepFrequenciesPerMinute = minutes.iterator.map(minute => SleepFrequency(minute, findSleepFrequency(minute, allIntervals)))
    sleepFrequenciesPerMinute.maxBy(_.sleepFrequency)
  }

  private def findSleepFrequency(minute: Int, sleepIntervals: Seq[SleepInterval]): Int = {
    sleepIntervals.count { case (from, to) => (from until to).contains(minute) }
  }

  private def calcTotalMinutesAsleep(shifts: Seq[Shift]): Int = {
    val minutesAsleep = for {
      shift <- shifts.iterator
      (from, to) <- shift.sleepIntervals.iterator
    } yield to - from

    minutesAsleep.sum
  }

  private def parseShifts(input: String): Seq[Shift] = {
    import fastparse._
    import MultiLineWhitespace._
    import adventOfCode.utils.parse.{num, digit, parseValue}

    def digits[_: P](n: Int) = P(digit.rep(exactly = n))

    def date[_: P] = P(digits(4) ~ "-" ~ digits(2) ~ "-" ~ digits(2))
    def minute[_: P] = P(digits(2) ~ ":" ~ digits(2).!.map(_.toInt))
    def dateTime[_: P] = P("[" ~ date ~ minute ~ "]")
    def shift[_: P] = P(dateTime ~ "Guard #" ~ num ~ "begins" ~ "shift").map { case (_, guardId) => guardId }
    def asleep[_: P] = P(dateTime ~ "falls" ~ "asleep")
    def wakeup[_: P] = P(dateTime ~ "wakes" ~ "up")

    def guard[_: P] = (shift ~ (asleep ~ wakeup).rep()).map(Shift tupled _)
    def parser[_: P] = guard.rep(1)

    parseValue(input, parser(_))
  }

  def implTests(): Unit = {
    import utest._

    val guard10 = """[1518-11-01 00:00] Guard #10 begins shift
                    |[1518-11-01 00:05] falls asleep
                    |[1518-11-01 00:25] wakes up
                    |[1518-11-01 00:30] falls asleep
                    |[1518-11-01 00:55] wakes up""".stripMargin

    assertMatch(parseShifts(guard10)) { case Seq(Shift(10, Seq((5, 25), (30, 55)))) => }
  }

}
