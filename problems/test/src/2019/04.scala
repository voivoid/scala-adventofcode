package adventOfCode.problems.tests

object year2019_04 extends utest.TestSuite {

  import utest._
  import adventOfCode.problems.year2019.{problem04 => problem}

  val tests = Tests {
    test("impl tests") {
      import problem._

      nextIncreasingPassword("111111".toList) ==> "111112".toList
      nextIncreasingPassword("111119".toList) ==> "111122".toList
      nextIncreasingPassword("111199".toList) ==> "111222".toList
      nextIncreasingPassword("118999".toList) ==> "119999".toList
      nextIncreasingPassword("155333".toList) ==> "155555".toList
      nextIncreasingPassword("900000".toList) ==> "999999".toList
    }
  }
}
