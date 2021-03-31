package adventOfCode.problems
package year2017

object problem10 extends baseProblem {

  import adventOfCode.utils.collections.{Zipper, CycledZipper}

  override def solve1(input: Input): Int = {
    solve1(input, defaultNumsLen)
  }

  def solve1(input: Input, numsLen: Int): Int = {
    import adventOfCode.utils.algorithms.IteratorSplit

    val lengths = input.splitBy(',').map(_.toInt).toList
    val numsZippper = makeNumsZipper(numsLen)

    val resultNums = runRounds(lengths, numsZippper, 1)
    resultNums(0) * resultNums(1)
  }

  override def solve2(input: Input): String = {
    val lengths = (input.iterator.map(_.toInt) ++ Iterator(17, 31, 73, 47, 23)).toList
    val numsZippper = makeNumsZipper(defaultNumsLen)

    val sparseHash = runRounds(lengths, numsZippper, 64)
    val denseHash = sparseHash.grouped(16).map(_.reduce(_ ^ _))
    val strHash = denseHash.map(num => f"$num%02x").mkString

    strHash
  }

  private type NumsZipper = Zipper[Int]
  private def defaultNumsLen = 255

  private def makeNumsZipper(numsLen: Int): NumsZipper = {
    CycledZipper((0 to numsLen).toList)
  }

  private def runRounds(lengths: List[Int], nums: NumsZipper, rounds: Int): List[Int] = {
    import adventOfCode.utils.algorithms.IteratorLast

    val initialSkipSize = 0

    val (finalNums, _) = Iterator
      .iterate((nums, initialSkipSize), rounds + 1) {
        case (currentNums, currentSkip) => {
          runRound(lengths, currentNums, currentSkip)
        }
      }
      .last

    finalNums.list
  }

  @scala.annotation.tailrec
  private def runRound(lengths: List[Int], nums: NumsZipper, skipSize: Int): (NumsZipper, Int) = lengths match {
    case Nil => (nums, skipSize)
    case l :: ls => {
      runRound(ls, reverseLenElems(nums, l).next(skipSize), skipSize + 1)
    }
  }

  private def reverseLenElems(nums: NumsZipper, len: Int): NumsZipper = {
    copyFromRightToLeft(nums, nums.next(len - 1), len)
  }

  @scala.annotation.tailrec
  private def copyFromRightToLeft(numsLeft: NumsZipper, numsRight: NumsZipper, len: Int): NumsZipper = len match {
    case 0 => numsLeft
    case n => {
      copyFromRightToLeft(numsLeft.updateCurrent(numsRight.current).next, numsRight.prev, n - 1)
    }
  }

}
