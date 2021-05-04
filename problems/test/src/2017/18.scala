package adventOfCode.problems.tests

object year2017_18 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2017.{problem18 => problem}

  def input1 = """set a 1
                |add a 2
                |mul a a
                |mod a 5
                |snd a
                |set a 0
                |rcv a
                |jgz a -1
                |set a 1
                |jgz a -2""".stripMargin

  def input2 = """snd 1
                 |snd 2
                 |snd p
                 |rcv a
                 |rcv b
                 |rcv c
                 |rcv d
                 |""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 4
    }

    test("solve2 base cases") {
      problem.solve2(input2) ==> 3
    }
  }
}
