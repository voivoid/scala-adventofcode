import mill._, scalalib._, scalafmt._
import $file.tests

object problems extends ScalaModule with ScalafmtModule {
  override def scalaVersion = "2.13.6"
  override def scalacOptions = scalaOpts
  override def ivyDeps = Agg(
    ivy"com.lihaoyi::fastparse:2.3.2",
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.1",
    ivy"org.scala-lang.modules::scala-collection-contrib:0.2.2",
    ivy"org.scala-lang.modules:scala-reflect:${scalaVersion()}",
    ivy"org.typelevel::cats-core:2.2.0",
    ivy"com.typesafe.play::play-json:2.9.2")

  object test extends Tests with ScalafmtModule {
    override def scalaVersion = problems.scalaVersion
    override def moduleDeps = Seq(problems)
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.10")
    override def testFramework = "utest.runner.Framework"
  }

  object bench extends ScalaModule with ScalafmtModule {
    override def scalaVersion = problems.scalaVersion
    override def moduleDeps = Seq(problems, test)
    override def ivyDeps = Agg(ivy"com.storm-enroute::scalameter:0.20")
  }

}

object app extends ScalaModule with ScalafmtModule with tests.AppTests {
  override def scalaVersion = problems.scalaVersion
  override def moduleDeps = Seq(problems)

  override protected def runProblem(input: String) = T.task {
    val Array(year, problem, part) = input.split('_')

    val appAssembly = app.assembly().path
    val testInputsPath = problems.test.millSourcePath / "resources" / "input"

    val problemId = s"year${year}.problem${problem}_${part}"
    T.log.info(s"running ${problemId}")

    val inputFile = s"${year}_${problem}"
    val result = os.proc("java", "-jar", appAssembly, problemId, testInputsPath / inputFile).call()

    (T.ctx.log, problemId, result.out.chunks.mkString)
  }

  def problem(input: String) = T.command {
    val (_, _, output) = runProblem(input)()
    T.log.info(output)
  }
}

def formatAll() = T.command {
  problems.reformat()()
  problems.test.reformat()()
  app.reformat()()
}

def scalaOpts = Seq(
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
