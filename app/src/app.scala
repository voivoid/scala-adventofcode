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
    val solver = adventOfCode.problems.findSolver(problemId)
    val result = solver(input)

    println(result)
  }
}
