trait AppTests
{
  def test2015() = T.command {
    check(runProblem("2015_01_1")(), "280")
    check(runProblem("2015_01_2")(), "1797")

    check(runProblem("2015_02_1")(), "1586300")
    check(runProblem("2015_02_2")(), "3737498")

    check(runProblem("2015_03_1")(), "2592")
    check(runProblem("2015_03_2")(), "2360")
  }

  def test2016() = T.command {
    check(runProblem("2016_01_1")(), "273")
    check(runProblem("2016_01_2")(), "115")

    check(runProblem("2016_02_1")(), "78293")
    check(runProblem("2016_02_2")(), "AC8C8")

    check(runProblem("2016_03_1")(), "983")
    check(runProblem("2016_03_2")(), "1836")
  }

  def test2017() = T.command {
    check(runProblem("2017_01_1")(), "1034")
    check(runProblem("2017_01_2")(), "1356")

    check(runProblem("2017_02_1")(), "58975")
    check(runProblem("2017_02_2")(), "308")

    check(runProblem("2017_03_1")(), "438")
    check(runProblem("2017_03_2")(), "266330")

    check(runProblem("2017_04_1")(), "455")
    check(runProblem("2017_04_2")(), "186")
  }

  def test2018() = T.command {
    check(runProblem("2018_01_1")(), "411")
    check(runProblem("2018_01_2")(), "56360")

    check(runProblem("2018_02_1")(), "5681")
    check(runProblem("2018_02_2")(), "uqyoeizfvmbistpkgnocjtwld")

    check(runProblem("2018_03_1")(), "116491")
    check(runProblem("2018_03_2")(), "707")

    check(runProblem("2018_04_1")(), "39584")
    check(runProblem("2018_04_2")(), "55053")
  }

  def test2019() = T.command {
    check(runProblem("2019_01_1")(), "3231195")
    check(runProblem("2019_01_2")(), "4843929")

    check(runProblem("2019_02_1")(), "5866663")
    check(runProblem("2019_02_2")(), "4259")

    check(runProblem("2019_03_1")(), "896")
    check(runProblem("2019_03_2")(), "16524")

    check(runProblem("2019_04_1")(), "1169")
    check(runProblem("2019_04_2")(), "757")
  }

  def test2020() = T.command {
    check(runProblem("2020_01_1")(), "744475")
    check(runProblem("2020_01_2")(), "70276940")

    check(runProblem("2020_02_1")(), "460")
    check(runProblem("2020_02_2")(), "251")

    check(runProblem("2020_03_1")(), "262")
    check(runProblem("2020_03_2")(), "2698900776")

    check(runProblem("2020_04_1")(), "256")
    check(runProblem("2020_04_2")(), "198")

    check(runProblem("2020_05_1")(), "866")
    check(runProblem("2020_05_2")(), "583")

    check(runProblem("2020_06_1")(), "6259")
    check(runProblem("2020_06_2")(), "3178")
  }

  def testAll() = T.command {
    test2015()()
    test2016()()
    test2017()()
    test2018()()
    test2019()()
    test2020()()
  }

  protected def runProblem(input: String): mill.define.Task[(mill.api.Logger, String, String)]

  private def check(result: (mill.api.Logger, String, String), expected: String): Unit = {
    val (logger, problemId, output) = result
    if (output.trim == expected) {
      logger.info(s"${problemId} OK")
    }
    else {
      logger.error(s"${problemId} FAILED; expected ${expected}; got ${output}")
    }
  }

}

