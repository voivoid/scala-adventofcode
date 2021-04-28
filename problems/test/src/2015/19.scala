package adventOfCode.problems.tests

object year2015_19 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem19 => problem}

  def input1 = """H => HO
                |H => OH
                |O => HH
                |
                |HOH""".stripMargin

  def input2 = """H => HO
                 |H => OH
                 |O => HH
                 |
                 |HOHOHO""".stripMargin

  def input3 = """e => H
                 |e => O
                 |H => HO
                 |H => OH
                 |O => HH
                 |
                 |HOH""".stripMargin

  def input4 = """e => H
                 |e => O
                 |H => HO
                 |H => OH
                 |O => HH
                 |
                 |HOHOHO""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 4
      problem.solve1(input2) ==> 7
    }

    test("solve2 base cases") {
      problem.solve2(input3) ==> 3
      problem.solve2(input4) ==> 6
    }
  }
}
