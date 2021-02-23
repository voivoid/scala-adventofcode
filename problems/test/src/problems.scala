package adventOfCode.problems.tests

import scala.language.implicitConversions

abstract class BaseTests extends utest.TestSuite {
  implicit def str2Input(s: String) = scala.io.Source.fromString(s)
}

object Input {
  def getURL(problem: String) = getClass.getClassLoader.getResource("input/" + problem)
}
