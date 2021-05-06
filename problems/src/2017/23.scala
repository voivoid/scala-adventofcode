package adventOfCode.problems
package year2017

object problem23 extends baseProblem {

  import problem18._

  override def solve1(input: Input): Int = {
    import adventOfCode.utils.algorithms.IteratorSlidingTuple

    val instructions = input.getLines().map(parseInstruction).toVector

    Iterator
      .iterate(makeInitialSerialState)(runSerialInstruction(instructions, _))
      .sliding2
      .takeWhile { case (s1, s2) => s1.ip != s2.ip }
      .count {
        case (s, _) => {
          val instruction = instructions(s.ip)
          instruction.isInstanceOf[Mul]
        }
      }
  }

  override def solve2(input: Input): Int = {
    val firstInstruction = parseInstruction(input.getLines().next())
    val b = firstInstruction match {
      case Set(reg, Value(value)) => { assert(reg.name == 'b'); value }
      case _                      => sys.error("unexpected")
    }

    val from = b * 100 + 100000
    val to = from + 17000
    val step = 17L
    calcComposites(from, to, step)
  }

  private def isComposite(n: Long): Boolean = {
    val bi = java.math.BigInteger.valueOf(n)
    !bi.isProbablePrime(20)
  }

  private def calcComposites(from: Long, to: Long, step: Long): Int = {
    (from to to by step).count(isComposite(_))
  }

}
