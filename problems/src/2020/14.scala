package adventOfCode.problems
package year2020

object problem14 extends baseProblem {

  override def solve1(input: Input): Long = {
    solve(input, State1(UniMask(0, 0), Map.empty))
  }

  override def solve2(input: Input): Long = {
    solve(input, State2(List.empty, Map.empty))
  }

  private def solve(input: Input, initialState: State): Long = {
    val instructions = input.getLines().map(parseInstruction)

    val finalState = instructions.foldLeft(initialState) { case (state, instruction) =>
      state.runInstruction(instruction)
    }

    finalState.memory.values.sum
  }

  private case class UniMask(orMask: Long, andMask: Long) {
    def apply(v: Long): Long = (v | orMask) & andMask
  }

  private type MemMap = Map[Long, Long]
  private sealed trait State {
    def memory: MemMap
    def runInstruction(instruction: Instruction): State
  }

  private case class State1(mask: UniMask, memory: MemMap) extends State {
    override def runInstruction(instruction: Instruction): State = {
      instruction match {
        case SetMask(mask) => {
          val orMask = java.lang.Long.parseLong(mask.replace('X', '0'), 2)
          val andMask = java.lang.Long.parseLong(mask.replace('X', '1'), 2)
          copy(mask = UniMask(orMask, andMask))
        }
        case WriteMem(addr, value) => {
          val maskedValue = mask(value)
          copy(memory = memory.updated(addr, maskedValue))
        }
      }
    }
  }

  private case class State2(masks: List[UniMask], memory: MemMap) extends State {
    override def runInstruction(instruction: Instruction): State = {
      instruction match {
        case SetMask(mask) => {
          copy(masks = genMaskCombinations(mask))
        }
        case WriteMem(addr, value) => {
          val memPairsToSet = masks.iterator.map { mask => mask(addr) -> value }
          copy(memory = memory ++ memPairsToSet)
        }
      }
    }
  }

  private sealed trait Instruction
  private case class SetMask(mask: String) extends Instruction
  private case class WriteMem(addr: Long, value: Long) extends Instruction

  private def parseInstruction(s: String): Instruction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, numL}

    def bits[_: P] = P("0" | "1" | "X").rep(36).!
    def mask[_: P]: P[Instruction] = P("mask" ~ "=" ~ bits).map(SetMask)
    def mem[_: P]: P[Instruction] = P("mem[" ~ numL ~ "]" ~ "=" ~ numL).map(WriteMem tupled _)
    def parser[_: P]: P[Instruction] = P(mask | mem)

    parseValue(s, parser(_))
  }

  private def genMaskCombinations(floatingMask: String): List[UniMask] = {
    val baseOrMask = java.lang.Long.parseLong(floatingMask.replace('X', '0'), 2)
    val floatingBitIndices = floatingMask.reverseIterator.zipWithIndex.filter { case (c, _) => c == 'X' }.map(_._2).toList

    generateCombinations(floatingBitIndices).map { case UniMask(orMask, andMask) => UniMask(orMask | baseOrMask, andMask) }
  }

  private def generateCombinations(bitIndices: List[Int]): List[UniMask] = bitIndices match {
    case Nil => List(UniMask(0, Long.MaxValue))
    case i :: is => {
      val bit1 = 1L << i

      generateCombinations(is).flatMap {
        case UniMask(orM, andM) => {
          List(UniMask(orM | bit1, andM), UniMask(orM, andM ^ bit1))
        }
      }
    }
  }

}
