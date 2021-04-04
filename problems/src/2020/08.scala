package adventOfCode.problems
package year2020

object problem08 extends baseProblem {

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.IteratorFindFirstDuplicate

    val instructions = input.getLines().map(parseInstruction).toVector

    val firstDupState = Iterator.iterate(initState)(runInstruction(_, instructions)).findFirstDuplicateElem.get
    firstDupState.acc
  }

  override def solve2(input: Input): Int = {
    val instructions = input.getLines().map(parseInstruction).toVector
    findAccAfterTerm(initState, visited = Set.empty, instructions, preforkState = None).getOrElse(sys.error("no solution"))
  }

  private def initState = State(ip = 0, acc = 0)

  @scala.annotation.tailrec
  private def findAccAfterTerm(
    state: State,
    visited: Set[IP],
    instructions: Instructions,
    preforkState: Option[(State, Set[IP])]
  ): Option[Accumulator] = {

    if (state.isTerminated(instructions)) {
      Some(state.acc)
    } else if (visited.contains(state.ip)) {
      preforkState match {
        case None                         => None
        case Some((oldState, oldVisited)) => findAccAfterTerm(oldState, oldVisited, instructions, None)
      }
    } else {
      val nextState = runInstruction(state, instructions)
      val nextVisited = visited + state.ip
      val currentInstruction = instructions(state.ip)
      val canFork = preforkState.isEmpty && currentInstruction.isPatchable

      if (canFork) {
        val preforkState = Some((nextState, nextVisited))
        findAccAfterTerm(state, visited, patchInstructions(state.ip, instructions), preforkState)
      } else {
        findAccAfterTerm(nextState, nextVisited, instructions, preforkState)
      }
    }
  }

  private def patchInstructions(ip: IP, instructions: Instructions): Instructions = {
    val instruction = instructions(ip)
    assert(instruction.isPatchable)

    instructions.updated(ip, instruction.patched.get)
  }

  private[problems] case class State(ip: IP, acc: Accumulator) {
    override def hashCode(): Int = ip
    override def equals(obj: Any): Boolean = obj match {
      case State(ip, _) => this.ip == ip
      case _            => false
    }

    def isTerminated(instructions: Instructions): Boolean = ip < 0 || ip >= instructions.size
  }

  private[problems] sealed trait Instruction {
    def nextState(state: State): State
    def patched: Option[Instruction]
    def isPatchable = patched.isDefined
  }

  private[problems] case class Nop(arg: Int) extends Instruction {
    override def nextState(state: State): State = state.copy(ip = state.ip + 1)
    override def patched: Option[Instruction] = Some(Jmp(arg))
  }

  private[problems] case class Acc(arg: Int) extends Instruction {
    override def nextState(state: State): State = state.copy(ip = state.ip + 1, acc = state.acc + arg)
    override def patched: Option[Instruction] = None
  }

  private[problems] case class Jmp(arg: Int) extends Instruction {
    override def nextState(state: State): State = state.copy(ip = state.ip + arg)
    override def patched: Option[Instruction] = Some(Nop(arg))
  }

  private type Instructions = Vector[Instruction]
  private type IP = Int
  private type Accumulator = Int

  private def runInstruction(state: State, instructions: Instructions): State = {
    instructions(state.ip).nextState(state)
  }

  private[problems] def parseInstruction(str: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}
    import Function.const

    def nop[_: P] = P("nop").map(const(Nop(_)))
    def acc[_: P] = P("acc").map(const(Acc(_)))
    def jmp[_: P] = P("jmp").map(const(Jmp(_)))
    def op[_: P] = P(nop | acc | jmp)

    def parser[_: P] = P(op ~ num).map { case (op, arg) => op(arg) }

    parseValue(str, parser(_))
  }

}
