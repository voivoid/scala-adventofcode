package adventOfCode.problems
package year2017

object problem08 extends baseProblem {

  override def solve1(input: Input): Int = {
    runAllInstructions(input).regMap.values.max
  }

  override def solve2(input: Input): Int = {
    runAllInstructions(input).maxRegValue
  }

  private def runAllInstructions(input: Input): RunResult = {
    val instructions = input.getLines().map(parseInstruction)

    val initRegValue = 0
    val initRegMap = Map.empty[Reg, Int].withDefaultValue(initRegValue)

    instructions.foldLeft(RunResult(initRegValue, initRegMap)) { case (RunResult(maxRegVal, regMap), instruction) =>
      runInstruction(maxRegVal, regMap, instruction)
    }
  }

  private type Reg = String
  private type RegValue = Int
  private type RegMap = Map[Reg, RegValue]
  private type CmpOp = RegValue => Boolean

  private case class RunResult(maxRegValue: RegValue, regMap: RegMap)
  private[problems] case class Instruction(reg: Reg, value: RegValue, condReg: Reg, condOp: CmpOp)

  private def runInstruction(maxRegVal: RegValue, regMap: RegMap, instruction: Instruction): RunResult = {
    val condValue = regMap(instruction.condReg)
    if (instruction.condOp(condValue)) {
      val updatedValue = regMap(instruction.reg) + instruction.value
      RunResult(updatedValue max maxRegVal, regMap.updated(instruction.reg, updatedValue))
    } else RunResult(maxRegVal, regMap)
  }

  private[problems] def parseInstruction(str: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num, alpha}
    import Function.const

    def reg[_: P] = P(alpha.repX(1).!)
    def inc[_: P] = P("inc" ~ num)
    def dec[_: P] = P("dec" ~ num).map(n => -n)
    def value[_: P] = P(inc | dec)

    type CMP = (Int, Int) => Boolean

    def lt[_: P]: P[CMP] = P("<").map(const(_ < _))
    def gt[_: P]: P[CMP] = P(">").map(const(_ > _))
    def eq[_: P]: P[CMP] = P("==").map(const(_ == _))
    def ne[_: P]: P[CMP] = P("!=").map(const(_ != _))
    def le[_: P]: P[CMP] = P("<=").map(const(_ <= _))
    def gr[_: P]: P[CMP] = P(">=").map(const(_ >= _))

    def cmpOp[_: P] = P((eq | ne | le | gr | lt | gt) ~ num).map { case (f, n) => f(_, n) }

    def parser[_: P] = P(reg ~ value ~ "if" ~ reg ~ cmpOp).map(Instruction tupled _)

    parseValue(str, parser(_))
  }

}
