import mill._, scalalib._, scalafmt._

object problems extends ScalaModule with ScalafmtModule {
  override def scalaVersion = "2.13.4"
  override def ivyDeps = Agg(ivy"org.parboiled::parboiled:2.2.1")

  object test extends Tests with ScalafmtModule {
    override def scalaVersion = problems.scalaVersion
    override def moduleDeps = Seq(problems)
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.7")
    override def testFrameworks = Seq("utest.runner.Framework")
  }
}

object app extends ScalaModule with ScalafmtModule {
  override def scalaVersion = problems.scalaVersion
  override def moduleDeps = Seq(problems)
  override def ivyDeps = Agg(ivy"org.scala-lang:scala-reflect:${ scalaVersion() }")

  def runProblem(input: String) = T.task {
    val Array(year, problem, part) = input.split('_')

    val appAssembly = app.assembly().path
    val testInputsPath = problems.test.millSourcePath / "resources" / "input"

    val problemId = s"year${ year }.problem${ problem }_${ part }"
    T.log.info(s"running ${ problemId }")
    val result = os.proc("java", "-jar", appAssembly,
                         problemId, testInputsPath / "2015_01").call()

    (problemId, result.out.chunks.mkString)
  }

  def problem(input: String) = T.command {
    val (_, output) = runProblem(input)()
    T.log.info(output)
  }

  def testAll() = T.command {
    def check(result: (String, String), expected: String): Unit = {
      val (problemId, output) = result
      if (output.trim == expected) {
        T.log.info(s"${ problemId } OK")
      }
      else {
        T.log.error(s"${ problemId } FAILED; expected ${ expected }; got ${ output }")
      }
    }

    check(runProblem("2015_01_1")(), "280")
    check(runProblem("2015_01_2")(), "1797")
  }
}
