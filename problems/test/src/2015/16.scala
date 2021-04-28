package adventOfCode.problems.tests

object year2015_16 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem16 => problem}

  val tests = Tests {
    test("impl tests") {
      import problem._

      assertMatch(problem.parseAunt("Sue 1: goldfish: 6, trees: 9, akitas: 0")) {
        case Aunt(1, map) if map == Map(Param.goldfish -> 6, Param.trees -> 9, Param.akitas -> 0) =>
      }
    }
  }
}
