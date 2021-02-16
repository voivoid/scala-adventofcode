object ProblemsApp {

  def main(args: Array[String]): Unit = {
    args.length match {
      case 1 => solveProblem(args(0), io.Source.stdin)
      case 2 => {
        val result = scala.util.Using(io.Source.fromFile(args(1))) { source =>
          solveProblem(args(0), source)
        }
        result.recover({ e => println(e.getMessage) })
      }
      case _ => showHelp()
    }
  }

  private def showHelp(): Unit = {
    println("Usage: ...") // TODO
  }

  private def solveProblem(problemId: String, input: scala.io.Source): Unit = {
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

    val result = problemPart match {
      case "_1" => problem.solve1(input)
      case "_2" => problem.solve2(input)
      case _    => sys.error("Unexpected part: " + problemPart)
    }

    println(result)
  }
}
