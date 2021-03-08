package adventOfCode.utils

package object intcode {

  type Code = Int
  type Memory = Vector[Code]
  type In = List[Code]
  type Out = List[Code]

  def parseMemory(input: scala.io.Source): Memory = {
    import adventOfCode.utils.algorithms.IteratorSplit
    input.splitBy(',').map(_.toInt).toVector
  }

  def run(memory: Memory, input: In = List.empty): Machine = {
    val initialState = Machine(memory, 0, input, List.empty)
    val finalState = runMachine(initialState)

    finalState
  }

  private def runMachine(machine: Machine): Machine = {
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

  private val Ops = Set(AddOp, MulOp, InputOp, OutputOp, TJmp, FJmp, Less, Equals, HaltOp)
}

package intcode {
  case class Machine(memory: Memory, instructionPointer: Int, input: In, output: Out) {
    def getCurrentOp = memory(instructionPointer)
    def getOpArg(argN: Int): Code = {
      getParameterMode(argN) match {
        case 0 => memory(instructionPointer + argN)
        case 1 => instructionPointer + argN
        case _ => sys.error("unexpected parameter mode")
      }
    }

    def getParameterMode(argN: Int): Int = {
      import adventOfCode.utils.algorithms.powInt
      val opCode = getCurrentOp
      opCode / powInt(10, argN + 1) % 10
    }

    def readMem(addr: Code): Code = memory(addr)
    def writeMem(addr: Code, value: Code): Machine = copy(memory = memory.updated(addr, value))
  }

  trait Op {
    val codeId: Code
    val length: Int

    def run(machine: Machine): Machine = {
      val updatedMachine = updateMachineState(machine, getArgs(machine))
      updatedMachine.copy(instructionPointer = updatedMachine.instructionPointer + length)
    }

    type Args
    def getArgs(machine: Machine): Args

    def updateMachineState(machine: Machine, args: Args): Machine
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

    override def updateMachineState(machine: Machine, toAddr: Code): Machine = {
      val inputCode = machine.input.head

      machine
        .writeMem(toAddr, inputCode)
        .copy(input = machine.input.tail)
    }
  }

  object OutputOp extends UnaryOp {
    override val codeId: Code = 4

    override def updateMachineState(machine: Machine, fromAddr: Code): Machine = {
      val outputValue = machine.readMem(fromAddr)
      machine.copy(output = outputValue :: machine.output)
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

  object HaltOp extends Op {
    override val codeId: Code = 99

    override val length: Int = 0
    override type Args = Unit
    override def getArgs(machine: Machine): Args = ()
    override def updateMachineState(machine: Machine, args: Args) = machine
  }

  abstract class ArithmeticOp(op: (Code, Code) => Code) extends TernaryOp {
    override def updateMachineState(machine: Machine, args: Args): Machine = {
      val (from1Addr, from2Addr, toAddr) = args
      val result = op(machine.readMem(from1Addr), machine.readMem(from2Addr))

      machine.writeMem(toAddr, result)
    }
  }

  abstract class JumpOp(cmpOp: Code => Boolean) extends BinaryOp {
    override def updateMachineState(machine: Machine, args: Args): Machine = {
      val (fromAddr, toAddr) = args
      val cmpResult = cmpOp(machine.readMem(fromAddr))

      if (cmpResult) {
        val nextIP = machine.readMem(toAddr) - length
        machine.copy(instructionPointer = nextIP)
      } else {
        machine
      }
    }
  }

  abstract class CmpOp(cmpOp: (Code, Code) => Boolean) extends TernaryOp {
    override def updateMachineState(machine: Machine, args: Args): Machine = {
      val (from1Addr, from2Addr, toAddr) = args
      val cmpResult = cmpOp(machine.readMem(from1Addr), machine.readMem(from2Addr))

      val valueResult = if (cmpResult) 1 else 0
      machine.writeMem(toAddr, valueResult)
    }
  }

}
