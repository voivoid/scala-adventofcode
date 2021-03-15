package adventOfCode.problems.tests

object year2016_06 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem06 => problem}

  def input = """eedadn
                |drvtee
                |eandsr
                |raavrd
                |atevrs
                |tsrnev
                |sdttsa
                |rasrtv
                |nssdts
                |ntnada
                |svetve
                |tesnvt
                |vntsnd
                |vrdear
                |dvrsen
                |enarar""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> "easter"
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> "advent"
    }
  }
}
