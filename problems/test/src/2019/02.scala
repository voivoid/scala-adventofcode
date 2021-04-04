package adventOfCode.problems.tests

object year2019_02 extends BaseTests {

  import utest._

  val tests = Tests {
    test("impl tests") {
      import adventOfCode.utils.intcode._

      run(Vector(1, 0, 0, 0, 99)).memory ==> Vector(2, 0, 0, 0, 99)
      run(Vector(2, 3, 0, 3, 99)).memory ==> Vector(2, 3, 0, 6, 99)
      run(Vector(2, 4, 4, 5, 99, 0)).memory ==> Vector(2, 4, 4, 5, 99, 9801)
      run(Vector(1, 1, 1, 4, 99, 5, 6, 0, 99)).memory ==> Vector(30, 1, 1, 4, 2, 5, 6, 0, 99)
    }
  }
}
