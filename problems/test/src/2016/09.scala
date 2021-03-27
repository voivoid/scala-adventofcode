package adventOfCode.problems.tests

object year2016_09 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem09 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("") ==> 0
      problem.solve1("ADVENT") ==> 6
      problem.solve1("A(1x5)BC") ==> 7
      problem.solve1("(3x3)XYZ") ==> 9
      problem.solve1("A(2x2)BCD(2x2)EFG") ==> 11
      problem.solve1("(6x1)(1x3)A") ==> 6
      problem.solve1("X(8x2)(3x3)ABCY") ==> 18
    }

    test("solve2 base cases") {
      problem.solve2("(3x3)XYZ") ==> 9
      problem.solve2("X(8x2)(3x3)ABCY") ==> 20
      problem.solve2("(27x12)(20x12)(13x14)(7x10)(1x12)A") ==> 241920
      problem.solve2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") ==> 445
    }
  }

}
