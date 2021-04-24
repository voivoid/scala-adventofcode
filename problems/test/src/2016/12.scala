package adventOfCode.problems.tests

object year2016_12 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem12 => problem}

  def input =
    """cpy 41 a
      |inc a
      |inc a
      |dec a
      |jnz a 2
      |dec a""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 42
    }

    test("impl tests") {
      import problem._

      assertMatch(problem.parseInstruction("cpy 41 a")) { case Cpy(Value(41), Reg('a')) => }
      assertMatch(problem.parseInstruction("inc b")) { case Inc(Reg('b')) => }
      assertMatch(problem.parseInstruction("dec c")) { case Dec(Reg('c')) => }
      assertMatch(problem.parseInstruction("jnz d 2")) { case Jnz(Reg('d'), Value(2)) => }
    }
  }
}
