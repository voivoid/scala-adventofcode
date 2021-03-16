package adventOfCode.problems
package year2015

object problem07 extends baseProblem {

  override def solve1(input: Input): Int = {
    val wireMap = parseWireMap(input)
    val (signalA, _) = calcWire("a", wireMap)

    signalA
  }

  override def solve2(input: Input): Int = {
    val wireMap = parseWireMap(input)
    val (originalSignalA, _) = calcWire("a", wireMap)

    val overridenWireMap = wireMap.updated("b", Value(originalSignalA))
    val (overridenSignalA, _) = calcWire("a", overridenWireMap)

    overridenSignalA
  }

  private type Signal = Int
  private type WireName = String
  private type WireMap = Map[WireName, Source]

  private sealed trait Source
  private case class Value(signal: Signal) extends Source
  private case class Wire(wire: WireName) extends Source

  private type UnaryFunc = Signal => Signal
  private case class UnaryOp(arg1: Source, f: UnaryFunc) extends Source

  private type BinaryFunc = (Signal, Signal) => Signal
  private case class BinaryOp(arg1: Source, arg2: Source, f: BinaryFunc) extends Source

  private def calcWire(wire: WireName, wireMap: WireMap): (Signal, WireMap) = {
    val source = wireMap(wire)
    val (resultSignal, resultMap) = calcSource(source, wireMap)

    (resultSignal, resultMap.updated(wire, Value(resultSignal)))
  }

  private def calcSource(source: Source, wireMap: WireMap): (Signal, WireMap) = {
    val (resultSignal, resultMap) = source match {
      case Value(signal) => (signal, wireMap)
      case Wire(wire)    => calcWire(wire, wireMap)

      case UnaryOp(arg1, op) => {
        val (arg1Signal, resultMap) = calcSource(arg1, wireMap)
        (op(arg1Signal), resultMap)
      }

      case BinaryOp(arg1, arg2, op) => {
        val (arg1Signal, resultMap1) = calcSource(arg1, wireMap)
        val (arg2Signal, resultMap2) = calcSource(arg2, resultMap1)
        (op(arg1Signal, arg2Signal), resultMap2)
      }
    }

    (resultSignal & 0xffff, resultMap)
  }

  private def parseWireMap(input: Input): WireMap = {
    input
      .getLines()
      .map(str => {
        val (source, wire) = parseInstruction(str)
        (wire, source)
      })
      .toMap
  }

  private def parseInstruction(str: String): (Source, WireName) = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{num, alpha, parseValue}

    def wireName[_: P]: P[WireName] = P(alpha.repX(1).!)

    def value[_: P]: P[Source] = P(num).map(Value(_))
    def wire[_: P]: P[Source] = P(wireName).map(Wire(_))
    def source[_: P]: P[Source] = P(value | wire)

    def unaryOp[_: P]: P[Source] = P("NOT" ~ source).map(arg => UnaryOp(arg, signal => ~signal))

    def andF[_: P]: P[BinaryFunc] = P("AND").map(_ => _ & _)
    def orF[_: P]: P[BinaryFunc] = P("OR").map(_ => _ | _)
    def lshiftF[_: P]: P[BinaryFunc] = P("LSHIFT").map(_ => _ << _)
    def rshiftF[_: P]: P[BinaryFunc] = P("RSHIFT").map(_ => _ >> _)

    def binaryOp[_: P]: P[Source] = P(source ~ (andF | orF | lshiftF | rshiftF) ~ source).map { case (arg1, f, arg2) =>
      BinaryOp(arg1, arg2, f)
    }

    def instruction[_: P] = P(binaryOp | unaryOp | source)

    def parser[_: P] = P(instruction ~ "->" ~ wireName)

    parseValue(str, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

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
