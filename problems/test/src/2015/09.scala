package adventOfCode.problems.tests

object year2015_09 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem09 => problem}

  def input = """London to Dublin = 464
                |London to Belfast = 518
                |Dublin to Belfast = 141""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 605
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 982
    }

    test("impl tests") {
      import problem._
      assertMatch(parseRoute("London to Dublin = 464")) { case Route("London", "Dublin", 464) => }
    }
  }
}
