import org.scalameter.api._

object ProblemBenchmark extends Bench.LocalTime {
  // TODO: read from command line args
  val year = 2019
  val problem = "02"
  val part = 2

  val inputFile = adventOfCode.problems.tests.Input.file(s"${year}_${problem}")
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
