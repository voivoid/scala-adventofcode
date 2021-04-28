package adventOfCode.problems.tests

object year2015_14 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem14 => problem}

  def input = """Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
                |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input, 1000) ==> 1120
    }

    test("solve2 base cases") {
      problem.solve2(input, 1000) ==> 689
    }

    test("impl tests") {
      import problem._

      assertMatch(parseDeer("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.")) {
        case Deer("Comet", 14, 10, 127) =>
      }
    }
  }
}
