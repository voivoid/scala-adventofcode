package adventOfCode.problems.tests

object year2015_07 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem07 => problem}

  val tests = Tests {
    test("impl tests") {
      import problem._

      assertMatch(parseInstruction("123 -> x")) { case (Value(123), "x") => }
      assertMatch(parseInstruction("y -> x")) { case (Wire("y"), "x") => }
      assertMatch(parseInstruction("x AND y -> d")) { case (BinaryOp(Wire("x"), Wire("y"), _), "d") => }
      assertMatch(parseInstruction("NOT x -> h")) { case (UnaryOp(Wire("x"), _), "h") => }

      def input = """123 -> x
                    |456 -> y
                    |x AND y -> d
                    |x OR y -> e
                    |x LSHIFT 2 -> f
                    |y RSHIFT 2 -> g
                    |NOT x -> h
                    |NOT y -> i""".stripMargin

      val wireMap = parseWireMap(scala.io.Source.fromString(input))
      calcWire("x", wireMap)._1 ==> 123
      calcWire("y", wireMap)._1 ==> 456
      calcWire("d", wireMap)._1 ==> 72
      calcWire("e", wireMap)._1 ==> 507
      calcWire("f", wireMap)._1 ==> 492
      calcWire("g", wireMap)._1 ==> 114
      calcWire("h", wireMap)._1 ==> 65412
      calcWire("i", wireMap)._1 ==> 65079
    }
  }
}
