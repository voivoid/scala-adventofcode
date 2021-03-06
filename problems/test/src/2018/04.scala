package adventOfCode.problems.tests

object year2018_04 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem04 => problem}

  def input = """[1518-11-01 00:00] Guard #10 begins shift
                |[1518-11-01 00:05] falls asleep
                |[1518-11-01 00:25] wakes up
                |[1518-11-01 00:30] falls asleep
                |[1518-11-01 00:55] wakes up
                |[1518-11-01 23:58] Guard #99 begins shift
                |[1518-11-02 00:40] falls asleep
                |[1518-11-02 00:50] wakes up
                |[1518-11-03 00:05] Guard #10 begins shift
                |[1518-11-03 00:24] falls asleep
                |[1518-11-03 00:29] wakes up
                |[1518-11-04 00:02] Guard #99 begins shift
                |[1518-11-04 00:36] falls asleep
                |[1518-11-04 00:46] wakes up
                |[1518-11-05 00:03] Guard #99 begins shift
                |[1518-11-05 00:45] falls asleep
                |[1518-11-05 00:55] wakes up""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 240
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 4455
    }

    test("impl tests") {
      import problem._

      val guard10 = """[1518-11-01 00:00] Guard #10 begins shift
                      |[1518-11-01 00:05] falls asleep
                      |[1518-11-01 00:25] wakes up
                      |[1518-11-01 00:30] falls asleep
                      |[1518-11-01 00:55] wakes up""".stripMargin

      assertMatch(parseShifts(guard10)) { case Seq(Shift(10, Seq((5, 25), (30, 55)))) => }
    }
  }
}
