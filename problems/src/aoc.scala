package adventOfCode

package problems {
  trait baseProblem
  {
    type Input = scala.io.Source

    def solve1(input: Input): Any
    def solve2(input: Input): Any
  }
}

