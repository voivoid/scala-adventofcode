package adventOfCode.problems.tests

object year2016_07 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem07 => problem}

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1("abba[mnop]qrst") ==> 1
      problem.solve1("abcd[bddb]xyyx") ==> 0
      problem.solve1("aaaa[qwer]tyui") ==> 0
      problem.solve1("ioxxoj[asdfgh]zxcvbn") ==> 1
    }

    test("solve2 base cases") {
      problem.solve2("aba[bab]xyz") ==> 1
      problem.solve2("xyx[xyx]xyx") ==> 0
      problem.solve2("aaa[kek]eke") ==> 1
      problem.solve2("zazbz[bzb]cdb") ==> 1
    }
  }
}
