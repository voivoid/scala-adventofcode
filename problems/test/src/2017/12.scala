package adventOfCode.problems.tests

object year2017_12 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem12 => problem}

  def input = """0 <-> 2
                |1 <-> 1
                |2 <-> 0, 3, 4
                |3 <-> 2, 4
                |4 <-> 2, 3, 6
                |5 <-> 6
                |6 <-> 4, 5""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 6
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 2
    }

    test("impl tests") {
      import problem._

      assertMatch(parseConnections("2 <-> 0, 3, 4")) { case Connection(2, List(0, 3, 4)) =>
      }
    }
  }
}
