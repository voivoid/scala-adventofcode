package adventOfCode.problems.tests

object year2015_24 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem24 => problem}

  def input = "1,2,3,4,5,7,8,9,10,11".replace(',', '\n')

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 99
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 44
    }
  }
}
