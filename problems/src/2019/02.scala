package adventOfCode.problems
package year2019

object problem02 extends baseProblem {

  override def solve1(input: Input): Int = {
    run(parseCodes(input), noun = 12, verb = 2)
  }

  override def solve2(input: Input): Int = {
    val codes = parseCodes(input)
    val resultToFind = 19690720

    val solutions = for {
      noun <- 0 to 99
      verb <- 0 to 99
      if run(codes, noun, verb) == resultToFind
    } yield 100 * noun + verb

    solutions.headOption.getOrElse(sys.error("no solution"))
  }

  private type Code = Int
  private type Codes = Array[Code]

  private case class State(codes: Codes, instructionPointer: Int)

  private def parseCodes(input: Input): Codes = {
    import adventOfCode.utils.algorithms.IteratorSplit
    input.splitBy(',').map(_.toInt).toArray
  }

  private def run(initialCodes: Codes, noun: Code, verb: Code): Code = {
    val patchedCodes = initialCodes.updated(1, noun).updated(2, verb)

    val initialState = State(patchedCodes, 0)
    val finalState = run(initialState)

    finalState.codes(0)
  }

  private def run(state: State): State = {
    import adventOfCode.utils.algorithms.IteratorSlidingTuple

    val states = Iterator.iterate(state)(calcNextState)

    // a machine is stopped if the instruction pointer is not changed in between two consecutive states
    val finalStates = states.sliding2
      .find { case (State(_, i1), State(_, i2)) => i1 == i2 }
      .getOrElse(sys.error("this can never happen"))

    finalStates._1
  }

  private def calcNextState(state: State): State = {
    val currentOpCode = OpCode(state.codes(state.instructionPointer))

    import OpCode._
    currentOpCode match {
      case Add  => addInstruction(state)
      case Mul  => mulInstruction(state)
      case Halt => state
      case _    => sys.error("unexpected opcode")
    }
  }

  private def addInstruction(state: State): State = {
    assert(state.codes(state.instructionPointer) == OpCode.Add.id)
    binaryOpInstruction(state, _ + _)
  }

  private def mulInstruction(state: State): State = {
    assert(state.codes(state.instructionPointer) == OpCode.Mul.id)
    binaryOpInstruction(state, _ * _)
  }

  private def binaryOpInstruction(state: State, op: (Code, Code) => Code): State = {
    val codes = state.codes
    val IP = state.instructionPointer

    val arg1 = codes(codes(IP + 1))
    val arg2 = codes(codes(IP + 2))
    val to = codes(IP + 3)

    val result = op(arg1, arg2)

    val updatedCodes = codes.updated(to, result)
    val nextIP = state.instructionPointer + 4
    State(updatedCodes, nextIP)
  }

  private object OpCode extends scala.Enumeration {
    val Add = Value(1)
    val Mul = Value(2)
    val Halt = Value(99)

    type OpCode = Value
  }

  def implTests(): Unit = {
    import utest._

    {
      assertMatch(run(State(Array(1, 0, 0, 0, 99), 0))) { case State(Array(2, 0, 0, 0, 99), 4) => }
      assertMatch(run(State(Array(2, 3, 0, 3, 99), 0))) { case State(Array(2, 3, 0, 6, 99), 4) => }
      assertMatch(run(State(Array(2, 4, 4, 5, 99, 0), 0))) { case State(Array(2, 4, 4, 5, 99, 9801), 4) => }
      assertMatch(run(State(Array(1, 1, 1, 4, 99, 5, 6, 0, 99), 0))) { case State(Array(30, 1, 1, 4, 2, 5, 6, 0, 99), 8) => }
    }
  }

}
