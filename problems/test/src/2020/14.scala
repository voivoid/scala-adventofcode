package adventOfCode.problems.tests

object year2020_14 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem14 => problem}

  def input1 = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                |mem[8] = 11
                |mem[7] = 101
                |mem[8] = 0""".stripMargin

  def input2 = """mask = 000000000000000000000000000000X1001X
                 |mem[42] = 100
                 |mask = 00000000000000000000000000000000X0XX
                 |mem[26] = 1""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 165
    }

    test("solve2 base cases") {
      problem.solve2(input2) ==> 208
    }

  }
}
