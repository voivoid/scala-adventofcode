package adventOfCode.problems
package year2016

object problem12 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, InitialRegMap)
  }

  override def solve2(input: Input): Int = {
    solve(input, InitialRegMap.updated('c', 1))
  }

  private def solve(input: Input, initialRegMap: RegMap): Int = {
    val instructions = input.getLines().map(parseInstruction).toVector
    val initialState = State(0, initialRegMap)

    val finalState = runInstructions(initialState, instructions)
    finalState.regMap('a')
  }

  private type RegName = Char
  private type ValInt = Int

  private[problems] sealed trait Source
  private[problems] case class Value(value: ValInt) extends Source
  private[problems] case class Reg(name: RegName) extends Source

  private[problems] sealed trait Instruction {
    def exec(state: State): State = {
      val (ipInc, nextState) = run(state)
      nextState.copy(ip = nextState.ip + ipInc)
    }

    protected def run(state: State): (Int, State)
  }

  private[problems] case class Cpy(source: Source, reg: Reg) extends Instruction {
    override def run(state: State): (Int, State) = {
      (1, state.copy(regMap = state.regMap.updated(reg.name, read(source, state))))
    }
  }

  private[problems] case class Inc(reg: Reg) extends Instruction {
    override def run(state: State): (Int, State) = {
      (1, incRegValue(state, reg, 1))
    }
  }

  private[problems] case class Dec(reg: Reg) extends Instruction {
    override def run(state: State): (Int, State) = {
      (1, incRegValue(state, reg, -1))
    }
  }

  private[problems] case class Jnz(cond: Source, offset: Source) extends Instruction {
    override def run(state: State): (Int, State) = {
      val ipInc =
        if (read(cond, state) == 0) 1
        else read(offset, state)

      (ipInc, state)
    }
  }

  private type Instructions = Vector[Instruction]

  private def incRegValue(state: State, reg: Reg, inc: Int): State = {
    val regName = reg.name
    val newRegValue = state.regMap(regName) + inc
    val newRegMap = state.regMap.updated(regName, newRegValue)
    state.copy(regMap = newRegMap)
  }

  private[problems] def parseInstruction(s: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def reg[_: P]: P[Reg] = P(CharIn("abcd")).!.map { s => assert(s.length == 1); Reg(s.head) }
    def value[_: P]: P[Value] = P(num).map(Value)
    def source[_: P]: P[Source] = P(reg | value)

    def cpy[_: P] = P("cpy" ~ source ~ reg).map(Cpy tupled _)
    def inc[_: P] = P("inc" ~ reg).map(Inc)
    def dec[_: P] = P("dec" ~ reg).map(Dec)
    def jnz[_: P] = P("jnz" ~ source ~ source).map(Jnz tupled _)

    def parser[_: P] = P(cpy | inc | dec | jnz)

    parseValue(s, parser(_))
  }

  private def InitialRegMap = Map('a' -> 0, 'b' -> 0, 'c' -> 0, 'd' -> 0)

  private type RegMap = Map[RegName, ValInt]
  private[problems] case class State(ip: Int, regMap: RegMap)

  @scala.annotation.tailrec
  private def runInstructions(state: State, instructions: Instructions): State = {
    if (!instructions.indices.contains(state.ip)) state
    else {
      val currentInstruction = instructions(state.ip)
      val nextState = currentInstruction.exec(state)

      runInstructions(nextState, instructions)
    }
  }

  private def read(source: Source, state: State): ValInt = source match {
    case Value(v) => v
    case Reg(r)   => state.regMap(r)
  }

}
