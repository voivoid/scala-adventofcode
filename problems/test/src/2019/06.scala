package adventOfCode.problems.tests

object year2019_06 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem06 => problem}

  def input1 = """COM)B
                |B)C
                |C)D
                |D)E
                |E)F
                |B)G
                |G)H
                |D)I
                |E)J
                |J)K
                |K)L""".stripMargin

  def input2 = input1 + "\nK)YOU\nI)SAN"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 42
    }

    test("solve2 base cases") {
      problem.solve2(input2) ==> 4
    }
  }
}
