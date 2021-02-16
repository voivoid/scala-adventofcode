import mill._, scalalib._, scalafmt._

object problems extends ScalaModule with ScalafmtModule {
  override def scalaVersion = "2.13.4"

  override def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.2.2",
    ivy"com.lihaoyi::utest:0.7.7",
    ivy"org.typelevel::cats-core:2.2.0")

  override def scalacOptions = Seq(
    "-deprecation",
    "-Xfatal-warnings",
    "-explaintypes",
    "-feature",
    "-unchecked",
    "-Xcheckinit",
    "-Xlint:adapted-args",
    "-Xlint:constant",
    "-Xlint:delayedinit-select",
    "-Xlint:doc-detached",
    "-Xlint:inaccessible",
    "-Xlint:infer-any",
    "-Xlint:missing-interpolator",
    "-Xlint:nullary-unit",
    "-Xlint:option-implicit",
    "-Xlint:package-object-classes",
    "-Xlint:poly-implicit-overload",
    "-Xlint:private-shadow",
    "-Xlint:stars-align",
    "-Xlint:type-parameter-shadow",
    "-Xlint:nonlocal-return",
    "-Xlint:implicit-not-found",
    "-Xlint:serial",
    "-Xlint:valpattern",
    "-Xlint:eta-zero",
    "-Xlint:eta-sam",
    "-Xlint:deprecation",
    "-Wdead-code",
    "-Wextra-implicit",
    "-Wnumeric-widen",
    "-Woctal-literal",
    "-Wunused:imports",
    "-Wunused:privates,locals,implicits,patvars,params,explicits",
    "-Wvalue-discard"
  )

  object test extends Tests with ScalafmtModule {
    override def scalaVersion = problems.scalaVersion

    override def moduleDeps = Seq(problems)

    override def testFrameworks = Seq("utest.runner.Framework")
  }

}

object app extends ScalaModule with ScalafmtModule {
  override def scalaVersion = problems.scalaVersion

  override def moduleDeps = Seq(problems)

  override def ivyDeps = Agg(ivy"org.scala-lang:scala-reflect:${scalaVersion()}")

  def runProblem(input: String) = T.task {
    val Array(year, problem, part) = input.split('_')

    val appAssembly = app.assembly().path
    val testInputsPath = problems.test.millSourcePath / "resources" / "input"

    val problemId = s"year${year}.problem${problem}_${part}"
    T.log.info(s"running ${problemId}")

    val inputFile = s"${year}_${problem}"
    val result = os.proc("java", "-jar", appAssembly, problemId, testInputsPath / inputFile).call()

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
        T.log.info(s"${problemId} OK")
      }
      else {
        T.log.error(s"${problemId} FAILED; expected ${expected}; got ${output}")
      }
    }

    // 2015
    check(runProblem("2015_01_1")(), "280")
    check(runProblem("2015_01_2")(), "1797")

    // 2016
    check(runProblem("2016_01_1")(), "273")
    check(runProblem("2016_01_2")(), "115")

    // 2017
    check(runProblem("2017_01_1")(), "1034")
    check(runProblem("2017_01_2")(), "1356")

    // 2018
    check(runProblem("2018_01_1")(), "411")
    check(runProblem("2018_01_2")(), "56360")

    // 2019
    check(runProblem("2019_01_1")(), "3231195")
    check(runProblem("2019_01_2")(), "4843929")


  }
}

def formatAll() = T.command {
  problems.reformat()()
  problems.test.reformat()()
  app.reformat()()
}
