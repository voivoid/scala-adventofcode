package adventOfCode.problems.tests

object year2017_10 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem10 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("3,4,1,5", 4) ==> 12
    }

    test("solve2 base cases") {
      problem.solve2("") ==> "a2582a3a0e66e6e86e3812dcb672a272"
      problem.solve2("AoC 2017") ==> "33efeb34ea91902bb2f59c9920caa6cd"
      problem.solve2("1,2,3") ==> "3efbe78a8d82f29979031a4aa0b16a9d"
      problem.solve2("1,2,4") ==> "63960835bcdc130f0b66d7ff4f6a5a8e"
    }
  }
}
