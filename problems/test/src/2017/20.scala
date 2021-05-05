package adventOfCode.problems.tests

object year2017_20 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem20 => problem}

  def input1 = """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
                |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>""".stripMargin

  def input2 = """p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
                 |p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
                 |p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
                 |p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 0
    }

    test("solve2 base cases") {
      problem.solve2(input2) ==> 1
    }
  }
}
