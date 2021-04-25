package adventOfCode.utils

package object intcode {

  type Code = Long
  type Memory = Vector[Code]
  type In = List[Code]
  type Out = List[Code]
  type IP = Code

  def parseMemory(source: scala.io.Source): Memory = {
    import adventOfCode.utils.algorithms.IteratorSplit
    source.splitBy(',').map(_.toLong).toVector
  }

  def run(memory: Memory, input: In = List.empty): Machine = {
    val finalState = runMachine(makeMachine(memory, input))
    finalState
  }

  def makeMachine(memory: Memory, input: In = List.empty): Machine = {
    Machine(memory, Map.empty, 0, input, List.empty, 0)
  }

  def resumeMachine(machine: Machine, input: In): Machine = {
    assert(machine.waitsForInput)
    runMachine(machine.copy(input = machine.input ++ input))
  }

  def resetOutput(machine: Machine): Machine = {
    machine.copy(output = List.empty)
  }

  def runMachine(machine: Machine): Machine = {
    import adventOfCode.utils.algorithms.IteratorSlidingTuple

    val states = Iterator.iterate(machine)(calcNextState)

    // a machine is stopped if the instruction pointer is not changed in between two consecutive states
    val finalStates = states.sliding2
      .find { case (s1, s2) => s1.instructionPointer == s2.instructionPointer }
      .getOrElse(sys.error("this can never happen"))

    finalStates._1
  }

  private def calcNextState(machine: Machine): Machine = {
    decodeOp(machine).run(machine)
  }

  private def decodeOp(machine: Machine): Op = {
    val currentOpCode = machine.getCurrentOp % 100
    Ops.find(_.codeId == currentOpCode).getOrElse(sys.error("unexpected opcode"))
  }

  private val Ops = Set(AddOp, MulOp, InputOp, OutputOp, TJmp, FJmp, Less, Equals, RelBase, HaltOp)
}

package intcode {
  case class Machine(memory: Memory, extraMem: Map[Code, Code], instructionPointer: IP, input: In, output: Out, relativeBase: Code) {
    def getCurrentOp = readMem(instructionPointer)
    def getOpArg(argN: Int): Code = {
      getParameterMode(argN) match {
        case 0 => readMem(instructionPointer + argN) // position mode
        case 1 => instructionPointer + argN // immediate mode
        case 2 => relativeBase + readMem(instructionPointer + argN) // relative mode
        case _ => sys.error("unexpected parameter mode")
      }
    }

    def getParameterMode(argN: Int): Int = {
      import adventOfCode.utils.algorithms.powInt
      val opCode = getCurrentOp
      opCode.toInt / powInt(10, argN + 1) % 10
    }

    def readMem(from: Code): Code = {
      val intAddr = from.toInt
      if (memory.indices.contains(intAddr)) {
        memory(intAddr)
      } else {
        extraMem.getOrElse(from, 0)
      }
    }

    def writeMem(to: Code, value: Code): Machine = {
      val intAddr = to.toInt
      if (memory.indices.contains(intAddr)) {
        copy(memory = memory.updated(intAddr, value))
      } else {
        copy(extraMem = extraMem.updated(to, value))
      }
    }

    def waitsForInput: Boolean = getCurrentOp == InputOp.codeId
    def halted: Boolean = getCurrentOp == HaltOp.codeId
  }

  trait Op {
    val codeId: Code
    val length: Int

    def run(machine: Machine): Machine = {
      val (updatedMachine, nextIPOpt) = updateMachineState(machine, getArgs(machine))
      val nextIP = nextIPOpt.getOrElse(updatedMachine.instructionPointer + length)
      updatedMachine.copy(instructionPointer = nextIP)
    }

    type Args
    def getArgs(machine: Machine): Args

    def updateMachineState(machine: Machine, args: Args): (Machine, Option[IP])
  }

  trait UnaryOp extends Op {
    override type Args = Code
    override val length: Int = 2

    override def getArgs(machine: Machine): Args = machine.getOpArg(1)
  }

  trait BinaryOp extends Op {
    override type Args = (Code, Code)
    override val length: Int = 3

    override def getArgs(machine: Machine): Args = (machine.getOpArg(1), machine.getOpArg(2))
  }

  trait TernaryOp extends Op {
    override type Args = (Code, Code, Code)
    override val length: Int = 4

    override def getArgs(machine: Machine): Args = (machine.getOpArg(1), machine.getOpArg(2), machine.getOpArg(3))
  }

  object AddOp extends ArithmeticOp(_ + _) {
    override val codeId: Code = 1
  }

  object MulOp extends ArithmeticOp(_ * _) {
    override val codeId: Code = 2
  }

  object InputOp extends UnaryOp {
    override val codeId: Code = 3

    override def updateMachineState(machine: Machine, toAddr: Code): (Machine, Option[IP]) = {
      if (!machine.input.isEmpty) {
        val inputCode = machine.input.head
        val updatedMachine = machine
          .writeMem(toAddr, inputCode)
          .copy(input = machine.input.tail)

        (updatedMachine, None)
      } else {
        // if there is no more input force IP to stay unchanged so the machine will halt
        (machine, Some(machine.instructionPointer))
      }
    }
  }

  object OutputOp extends UnaryOp {
    override val codeId: Code = 4

    override def updateMachineState(machine: Machine, fromAddr: Code): (Machine, Option[IP]) = {
      val outputValue = machine.readMem(fromAddr)
      val updatedMachine = machine.copy(output = outputValue :: machine.output)

      (updatedMachine, None)
    }
  }

  object TJmp extends JumpOp(_ != 0) {
    override val codeId: Code = 5
  }

  object FJmp extends JumpOp(_ == 0) {
    override val codeId: Code = 6
  }

  object Less extends CmpOp(_ < _) {
    override val codeId: Code = 7
  }

  object Equals extends CmpOp(_ == _) {
    override val codeId: Code = 8
  }

  object RelBase extends UnaryOp {
    override val codeId: Code = 9

    override def updateMachineState(machine: Machine, fromAddr: Code): (Machine, Option[IP]) = {
      val relBase = machine.readMem(fromAddr)

      (machine.copy(relativeBase = machine.relativeBase + relBase), None)
    }
  }

  object HaltOp extends Op {
    override val codeId: Code = 99

    override val length: Int = 0
    override type Args = Unit
    override def getArgs(machine: Machine): Args = ()
    override def updateMachineState(machine: Machine, args: Args) = (machine, None)
  }

  abstract class ArithmeticOp(op: (Code, Code) => Code) extends TernaryOp {
    override def updateMachineState(machine: Machine, args: Args): (Machine, Option[IP]) = {
      val (from1Addr, from2Addr, toAddr) = args
      val result = op(machine.readMem(from1Addr), machine.readMem(from2Addr))

      (machine.writeMem(toAddr, result), None)
    }
  }

  abstract class JumpOp(cmpOp: Code => Boolean) extends BinaryOp {
    override def updateMachineState(machine: Machine, args: Args): (Machine, Option[IP]) = {
      val (fromAddr, toAddr) = args
      val cmpResult = cmpOp(machine.readMem(fromAddr))

      if (cmpResult) {
        val nextIP = machine.readMem(toAddr)
        (machine, Some(nextIP))
      } else {
        (machine, None)
      }
    }
  }

  abstract class CmpOp(cmpOp: (Code, Code) => Boolean) extends TernaryOp {
    override def updateMachineState(machine: Machine, args: Args): (Machine, Option[IP]) = {
      val (from1Addr, from2Addr, toAddr) = args
      val cmpResult = cmpOp(machine.readMem(from1Addr), machine.readMem(from2Addr))

      val valueResult = if (cmpResult) 1L else 0L
      (machine.writeMem(toAddr, valueResult), None)
    }
  }

}
