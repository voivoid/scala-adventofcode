package adventOfCode.problems.tests

import scala.language.implicitConversions

abstract class BaseTests extends utest.TestSuite {
  implicit def str2Input(s: String) = io.Source.fromString(s)
}
