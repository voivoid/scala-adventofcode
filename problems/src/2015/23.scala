package adventOfCode.problems
package year2015

object problem23 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, 0)
  }

  override def solve2(input: Input): Int = {
    solve(input, 1)
  }

  private def solve(input: Input, aReg: Int): Int = {
    val instructions = input.getLines().map(parseInstruction).toVector
    val state = State(aReg, 0, 0)

    val finalState = Iterator
      .iterate(state)(runInstruction(_, instructions))
      .find { case State(_, _, ip) =>
        !instructions.indices.contains(ip)
      }
      .get

    finalState.bReg
  }

  private def runInstruction(state: State, instructions: Vector[Instruction]): State = {
    val instruction = instructions(state.ip)

    val (offset, nextState) = instruction match {
      case HLF(reg)    => (1, state.updateReg(reg, _ / 2))
      case TPL(reg)    => (1, state.updateReg(reg, _ * 3))
      case INC(reg)    => (1, state.updateReg(reg, _ + 1))
      case JMP(offset) => (offset, state)
      case JIE(reg, offset) => {
        val shouldJump = (state.readReg(reg) % 2) == 0
        (if (shouldJump) offset else 1, state)
      }
      case JIO(reg, offset) => {
        val shouldJump = state.readReg(reg) == 1
        (if (shouldJump) offset else 1, state)
      }
    }

    nextState.copy(ip = nextState.ip + offset)
  }

  private case class State(aReg: Value, bReg: Value, ip: Int) {
    def readReg(reg: Char): Value = reg match {
      case 'a' => aReg
      case 'b' => bReg
      case _   => sys.error("unexpected")
    }

    def updateReg(reg: Char, f: Value => Value): State = {
      val nextVal = f(readReg(reg))
      reg match {
        case 'a' => copy(aReg = nextVal)
        case 'b' => copy(bReg = nextVal)
        case _   => sys.error("unexpected")
      }
    }
  }

  private[problems] type Reg = Char
  private[problems] type Value = Int
  private[problems] sealed trait Instruction

  private[problems] case class HLF(reg: Reg) extends Instruction
  private[problems] case class TPL(reg: Reg) extends Instruction
  private[problems] case class INC(reg: Reg) extends Instruction
  private[problems] case class JMP(offset: Value) extends Instruction
  private[problems] case class JIE(reg: Reg, offset: Value) extends Instruction
  private[problems] case class JIO(reg: Reg, offset: Value) extends Instruction

  private[problems] def parseInstruction(s: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def reg[_: P]: P[Reg] = P(CharIn("ab")).!.map(_.head)
    def hlf[_: P] = P("hlf" ~ reg).map(HLF)
    def tpl[_: P] = P("tpl" ~ reg).map(TPL)
    def inc[_: P] = P("inc" ~ reg).map(INC)
    def jmp[_: P] = P("jmp" ~ num).map(JMP)
    def jie[_: P] = P("jie" ~ reg ~ "," ~ num).map(JIE tupled _)
    def jio[_: P] = P("jio" ~ reg ~ "," ~ num).map(JIO tupled _)
    def parser[_: P] = P(hlf | tpl | inc | jmp | jie | jio)

    parseValue(s, parser(_))
  }

}
