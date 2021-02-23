package adventOfCode

package problems {
  trait baseProblem {
    type Input = scala.io.Source

    def solve1(input: Input): Any
    def solve2(input: Input): Any
  }
}

package object problems {
  type Problem = baseProblem#Input => Any

  def findSolver(problemId: String): Problem = { // TODO: use Either
    val (problemStr, problemPart) = problemId.splitAt(problemId.indexOf('_'))
    val problemModuleName = "adventOfCode.problems." + problemStr

    val problem =
      try {
        val runtimeMirror =
          scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)
        val module = runtimeMirror.staticModule(problemModuleName)
        runtimeMirror
          .reflectModule(module)
          .instance
          .asInstanceOf[adventOfCode.problems.baseProblem]
      } catch {
        case e: Throwable =>
          sys.error(s"Failed to load the problem ${problemStr}: ${e.getMessage}")
      }

    val solveF = problemPart match {
      case "_1" => problem.solve1 _
      case "_2" => problem.solve2 _
      case _    => sys.error("Unexpected part: " + problemPart)
    }

    solveF
  }
}