import org.scalameter.api._

object ProblemBenchmark extends Bench.LocalTime {
  // TODO: read from command line args
  val year = 2015
  val problem = "20"
  val part = 1

  val inputFile = adventOfCode.problems.tests.Input.getURL(s"${year}_${problem}")
  val solve = adventOfCode.problems.findSolver(s"year${year}.problem${problem}_${part}")

  performance of "Problem" in {
    measure method "solve" in {
      using(Gen.unit("problem")) in {
        _ => {
          val result = solve(scala.io.Source.fromURL(inputFile))
        }
      }
    }
  }


}
