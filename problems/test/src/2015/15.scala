package adventOfCode.problems.tests

object year2015_15 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2015.{problem15 => problem}

  def input = """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
                |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 62842880
    }

    test("solve2 base cases") {
      problem.solve2(input) ==> 57600000
    }

    test("impl tests") {
      import problem._

      assertMatch(parseIngredient("Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8")) {
        case Ingredient("Butterscotch", -1, -2, 6, 3, 8) =>
      }
    }
  }
}
