package adventOfCode.problems.tests

object year2017_11 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem11 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("ne,ne,ne") ==> 3
      problem.solve1("ne,ne,sw,sw") ==> 0
      problem.solve1("ne,ne,s,s") ==> 2
      problem.solve1("se,sw,se,sw,sw") ==> 3

      problem.solve1("se,ne,se,ne,se,ne") ==> 6
      problem.solve1("ne,se") ==> 2
      problem.solve1("ne,ne,s,s") ==> 2
    }

    test("solve2 base cases") {
      problem.solve2("ne,ne,ne") ==> 3
      problem.solve2("ne,ne,sw,sw") ==> 2
      problem.solve2("ne,ne,s,s") ==> 2
      problem.solve2("se,sw,se,sw,sw") ==> 3
    }
  }
}
