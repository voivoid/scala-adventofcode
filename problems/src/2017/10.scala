package adventOfCode.problems
package year2017

object problem10 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve1(input, DefaultNumsLen)
  }

  def solve1(input: Input, numsLen: Int): Int = {
    import adventOfCode.utils.algorithms.IteratorSplit

    val lengths = input.splitBy(',').map(_.toInt).toList
    val nums = makeNums(numsLen)

    val resultNums = runRounds(lengths, nums, 1)
    resultNums(0) * resultNums(1)
  }

  override def solve2(input: Input): String = {
    val lengths = input.iterator.map(_.toInt).toList ::: List(17, 31, 73, 47, 23)

    val sparseHash = runRounds(lengths, makeNums(DefaultNumsLen), 64)
    val denseHash = sparseHash.grouped(16).map(_.reduce(_ ^ _))
    val strHash = denseHash.map(num => f"$num%02x").mkString

    strHash
  }

  private type Nums = Vector[Int]
  private def DefaultNumsLen = 255
  private def makeNums(numsLen: Int): Nums = {
    (0 to numsLen).toVector
  }

  private def runRounds(lengths: List[Int], nums: Nums, rounds: Int): Vector[Int] = {
    import adventOfCode.utils.algorithms.IteratorLast

    val initialState = State(nums, 0, 0)

    val finalState = Iterator
      .iterate(initialState, rounds + 1)(runRound(lengths, _))
      .last

    finalState.nums
  }

  private case class State(nums: Nums, skipSize: Int, currentPos: Int)

  @scala.annotation.tailrec
  private def runRound(lengths: List[Int], state: State): State = lengths match {
    case Nil => state
    case len :: ls => {
      import state._

      val buff = nums.toBuffer
      for (idx <- 0 until len) {
        buff((currentPos + idx) % buff.size) = nums((currentPos + len - idx - 1) % buff.size)
      }

      runRound(ls, State(buff.toVector, skipSize + 1, currentPos + len + skipSize % nums.size))
    }
  }

}
