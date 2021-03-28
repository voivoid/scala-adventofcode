package adventOfCode.problems.tests

object year2020_10 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2020.{problem10 => problem}

  def input1 = """16
                 |10
                 |15
                 |5
                 |1
                 |11
                 |7
                 |19
                 |6
                 |12
                 |4""".stripMargin

  def input2 = """28
                 |33
                 |18
                 |42
                 |31
                 |14
                 |46
                 |20
                 |48
                 |47
                 |24
                 |23
                 |49
                 |45
                 |19
                 |38
                 |39
                 |11
                 |1
                 |32
                 |25
                 |35
                 |8
                 |17
                 |7
                 |9
                 |4
                 |2
                 |34
                 |10
                 |3""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 35
      problem.solve1(input2) ==> 220
    }

    test("solve2 base cases") {
      problem.solve2(input1) ==> 8
      problem.solve2(input2) ==> 19208
    }

  }
}
