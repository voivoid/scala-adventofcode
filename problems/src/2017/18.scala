package adventOfCode.problems
package year2017

object problem18 extends baseProblem {

  import scala.collection.immutable.Queue

  override def solve1(input: Input): Long = {
    val instructions = input.getLines().map(parseInstruction).toVector
    val initialState = SerialState(Map.empty[RegId, RegVal].withDefaultValue(0L), 0, None, None)

    val firstRcv = Iterator
      .iterate(initialState)(runSerialInstructions(instructions, _))
      .collectFirst { case state if state.lastRcv.isDefined => state.lastRcv.get }
      .get

    firstRcv
  }

  override def solve2(input: Input): Int = {
    val instructions = input.getLines().map(parseInstruction).toVector
    val initialState = ParallelState(makeProgramState(0), makeProgramState(1))

    val finalState = runParallelInstructions(instructions, initialState)
    finalState.p1.sndCounter
  }

  private def makeProgramState(prog: Long): ProgramState = {
    ProgramState(Map.empty[RegId, RegVal].withDefaultValue(0L).updated('p', prog), 0, Queue.empty[RegVal], 0)
  }

  private type IP = Int
  private type RegId = Char
  private type RegVal = Long
  private type RegMap = Map[RegId, RegVal]

  private case class SerialState(regMap: RegMap, ip: IP, lastSnd: Option[RegVal], lastRcv: Option[RegVal])
  private case class ProgramState(regMap: RegMap, ip: IP, out: Queue[RegVal], sndCounter: Int)
  private case class ParallelState(p0: ProgramState, p1: ProgramState)

  private sealed trait Source
  private case class Reg(r: RegId) extends Source
  private case class Value(v: RegVal) extends Source

  private sealed trait Instruction
  private case class Snd(x: Source) extends Instruction
  private case class Rcv(xR: Reg) extends Instruction
  private case class Set(xR: Reg, y: Source) extends Instruction
  private case class Add(xR: Reg, y: Source) extends Instruction
  private case class Mul(xR: Reg, y: Source) extends Instruction
  private case class Mod(xR: Reg, y: Source) extends Instruction
  private case class Jgz(x: Source, y: Source) extends Instruction

  private type Instructions = Vector[Instruction]

  private def runSerialInstructions(instructions: Instructions, state: SerialState): SerialState = {
    if (!instructions.indices.contains(state.ip)) state
    else {
      val (ipOffset, nextState) = runInstruction(instructions(state.ip), state)
      nextState.copy(ip = nextState.ip + ipOffset)
    }
  }

  private def runInstruction(instruction: Instruction, state: SerialState): (Int, SerialState) = instruction match {
    case Snd(x) => (1, state.copy(lastSnd = Some(readSource(x, state.regMap))))
    case Rcv(x) => {
      if (readSource(x, state.regMap) == 0) (1, state)
      else (1, state.copy(lastRcv = state.lastSnd))
    }

    case basicInstruction => {
      val (ipOffset, nextRegMap) = runBasicInstuction(basicInstruction, state.regMap)
      (ipOffset, state.copy(regMap = nextRegMap))
    }
  }

  private def runBasicInstuction(instruction: Instruction, regMap: RegMap): (IP, RegMap) = {
    val read = readSource(_, regMap)

    def upd(xR: Reg, y: Source, f: (Long, Long) => Long) =
      (1, regMap.updated(xR.r, f(read(xR), read(y))))

    instruction match {
      case Jgz(x, y) => {
        if (readSource(x, regMap) > 0) (read(y).toInt, regMap)
        else (1, regMap)
      }
      case Set(xR, y) => upd(xR, y, (_, v) => v)
      case Add(xR, y) => upd(xR, y, _ + _)
      case Mul(xR, y) => upd(xR, y, _ * _)
      case Mod(xR, y) => upd(xR, y, _ % _)
      case _          => sys.error("unexpected")
    }
  }

  @scala.annotation.tailrec
  private def runParallelInstructions(instructions: Instructions, state: ParallelState): ParallelState = {
    val i0 = instructions(state.p0.ip)
    val i1 = instructions(state.p1.ip)

    val (deadlock, nextState) = ((i0, state.p1.out), (i1, state.p0.out)) match {
      case ((_: Rcv, Queue()), (_: Rcv, Queue())) => (true, state) // deadlock

      case ((Rcv(xR), queue), _) if queue.nonEmpty => {
        val (nextP0, nextP1) = popQueueElemAndWriteItToReg(xR, queue, state.p0, state.p1)
        (false, ParallelState(nextP0, nextP1))
      }

      case (_, (Rcv(xR), queue)) if queue.nonEmpty => {
        val (nextP1, nextP0) = popQueueElemAndWriteItToReg(xR, queue, state.p1, state.p0)
        (false, ParallelState(nextP0, nextP1))
      }

      case (_, _) => {
        val nextP0 = i0 match {
          case _: Rcv => state.p0
          case _      => runInstruction(i0, state.p0)
        }

        val nextP1 = i1 match {
          case _: Rcv => state.p1
          case _      => runInstruction(i1, state.p1)
        }

        (false, ParallelState(nextP0, nextP1))
      }
    }

    if (deadlock) nextState
    else runParallelInstructions(instructions, nextState)
  }

  private def popQueueElemAndWriteItToReg(
    regToUpdate: Reg,
    queue: Queue[RegVal],
    regProgram: ProgramState,
    queueProgram: ProgramState
  ): (ProgramState, ProgramState) = {
    val (receivedValue, nextQueue) = queue.dequeue
    (runInstruction(Set(regToUpdate, Value(receivedValue)), regProgram), queueProgram.copy(out = nextQueue))
  }

  private def runInstruction(instruction: Instruction, state: ProgramState): ProgramState = {
    instruction match {
      case Snd(x) => {
        val v = readSource(x, state.regMap)
        state.copy(out = state.out.appended(v), ip = state.ip + 1, sndCounter = state.sndCounter + 1)
      }
      case i => {
        val (ipOffset, nextRegMap) = runBasicInstuction(i, state.regMap)
        state.copy(regMap = nextRegMap, ip = state.ip + ipOffset)
      }
    }
  }

  private def readSource(source: Source, regMap: RegMap): RegVal = source match {
    case Value(v) => v
    case Reg(r)   => regMap(r)
  }

  private def parseInstruction(s: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, numL, alpha}

    def reg[_: P] = P(alpha.map(Reg))
    def value[_: P] = P(numL).map(Value)
    def source[_: P] = P(reg | value)

    def snd[_: P] = P("snd" ~ source).map(Snd)
    def rcv[_: P] = P("rcv" ~ reg).map(Rcv)
    def set[_: P] = P("set" ~ reg ~ source).map(Set tupled _)
    def add[_: P] = P("add" ~ reg ~ source).map(Add tupled _)
    def mul[_: P] = P("mul" ~ reg ~ source).map(Mul tupled _)
    def mod[_: P] = P("mod" ~ reg ~ source).map(Mod tupled _)
    def jgz[_: P] = P("jgz" ~ source ~ source).map(Jgz tupled _)

    def parser[_: P] = P(snd | set | add | mul | mod | rcv | jgz)

    parseValue(s, parser(_))
  }

}
