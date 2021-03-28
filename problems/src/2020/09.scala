package adventOfCode.problems
package year2020

object problem09 extends baseProblem {

  import scala.collection.immutable.Queue

  override def solve1(input: Input): Long = {
    solve1(input, problemPreambleLen)
  }

  def solve1(input: Input, preambleLen: Int): Long = {
    val nums = input.getLines().map(_.toLong).toList
    val invalidNum = findFirstInvalidNum(nums, preambleLen)

    invalidNum.getOrElse(sys.error("no solution"))
  }

  override def solve2(input: Input): Long = {
    solve2(input, problemPreambleLen)
  }

  def solve2(input: Input, preambleLen: Int): Long = {
    val nums = input.getLines().map(_.toLong).toList

    val invalidNum = findFirstInvalidNum(nums, preambleLen).getOrElse(sys.error("no solution"))
    val contiguousNums = findContiguousNums(nums, invalidNum, Vector.empty, 0).getOrElse(sys.error("no solution"))

    import adventOfCode.utils.algorithms.IterableMinMax
    val (min, max) = contiguousNums.minmax
    min + max
  }

  private def problemPreambleLen = 25
  private type Num = Long

  @scala.annotation.tailrec
  private def findContiguousNums(nums: List[Num], invalidNum: Num, contNums: Vector[Num], sum: Num): Option[Vector[Num]] =
    nums match {
      case _ if sum == invalidNum => Some(contNums)
      case Nil                    => None
      case n :: ns if sum + n <= invalidNum => {
        findContiguousNums(ns, invalidNum, contNums.appended(n), sum + n)
      }
      case _ if contNums.isEmpty => None
      case _ => {
        val elemToDrop = contNums.head
        findContiguousNums(nums, invalidNum, contNums.drop(1), sum - elemToDrop)
      }
    }

  private def findFirstInvalidNum(nums: List[Num], preambleLen: Int): Option[Num] = {
    val (preamble, restNums) = nums.splitAt(preambleLen)

    val preambleQ = preamble.to(Queue)
    val preambleM = preambleQ.groupBy(identity).map { case (n, ns) => n -> ns.size }.toMap

    findFirstInvalidNum(restNums, preambleQ, preambleM)
  }

  private def findFirstInvalidNum(nums: List[Num], preambleQ: Queue[Num], preambleM: Map[Num, Int]): Option[Num] = nums match {
    case Nil => None
    case n :: ns => {
      if (!checkIfValid(n, preambleM)) {
        Some(n)
      } else {
        val (dequeuedElem, nextQ) = preambleQ.enqueue(n).dequeue

        val nextM = preambleM
          .updatedWith(n)(_.orElse(Some(0)).map(_ + 1))
          .updatedWith(dequeuedElem)(_.flatMap {
            case 1 => None
            case n => Some(n - 1)
          })

        findFirstInvalidNum(ns, nextQ, nextM)
      }
    }
  }

  private def checkIfValid(num: Num, preambleM: Map[Num, Int]): Boolean = {
    preambleM.exists {
      case (n1, _) => {
        val n2 = num - n1
        n1 != n2 && preambleM.exists { case (n, _) => n == n2 }
      }
    }
  }

}
