package adventOfCode.problems.tests

object year2018_11 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2018.{problem11 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("18") ==> "33,45"
    }

    test("solve2 base cases") {
      problem.solve2("18", 20) ==> "90,269,16"
    }

    test("test impl") {
      problem.calcFuelCell(3, 5, 8) ==> 4
      problem.calcFuelCell(122, 79, 57) ==> -5
      problem.calcFuelCell(217, 196, 39) ==> 0
      problem.calcFuelCell(101, 153, 71) ==> 4
    }
  }
}
