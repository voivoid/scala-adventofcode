package adventOfCode.problems.tests

object year2019_03 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem03 => problem}

  val input1 = "R8,U5,L5,D3\nU7,R6,D4,L4"
  val input2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"
  val input3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 6
      problem.solve1(input2) ==> 159
      problem.solve1(input3) ==> 135
    }

    test("solve2 base cases") {
      problem.solve2(input1) ==> 30
      problem.solve2(input2) ==> 610
      problem.solve2(input3) ==> 410
    }
  }
}
