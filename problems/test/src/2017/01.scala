package adventOfCode.problems.tests

object year2017_01 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem01 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("1122") ==> 3
      problem.solve1("1111") ==> 4
      problem.solve1("1234") ==> 0
      problem.solve1("91212129") ==> 9
    }

    test("solve2 base cases") {
      problem.solve2("1212") ==> 6
      problem.solve2("1221") ==> 0
      problem.solve2("123425") ==> 4
      problem.solve2("123123") ==> 12
      problem.solve2("12131415") ==> 4
    }
  }
}
