package adventOfCode.problems
package year2015

object problem24 extends baseProblem {

  override def solve1(input: Input): Long = {
    solve(input, 3)
  }

  override def solve2(input: Input): Long = {
    solve(input, 4)
  }

  private def solve(input: Input, groups: Int): Long = {
    val nums = input.getLines().map(_.toInt).toList
    val groupSum = nums.sum / groups

    val combinations = split(nums, groupSum)
    val entanglements = combinations.map(_.foldLeft(1L)(_ * _))

    entanglements.min
  }

  private def split(nums: List[Int], groupSum: Int): Iterator[List[Int]] = {
    Iterator
      .from(1)
      .map(combinationsNum => {
        nums
          .combinations(combinationsNum)
          .filter(combination => {
            combination.sum == groupSum
          })
      })
      .find(!_.isEmpty)
      .get
  }

}
