package adventOfCode.problems.tests

object year2018_10 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem10 => problem}

  val tests = Tests {
    test("impl tests") {
      import problem._

      assertMatch(parsePoint("position=<9,1> velocity=<0,2>")) { case Point(9, 1, 0, 2) => }
    }
  }
}
