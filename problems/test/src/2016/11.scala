package adventOfCode.problems.tests

object year2016_11 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2016.{problem11 => problem}

  def input = """The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
                |The second floor contains a hydrogen generator.
                |The third floor contains a lithium generator.
                |The fourth floor contains nothing relevant.""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input) ==> 11
    }

    test("impl tests") {
      import problem._

      assertMatch(parseFloor("The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.")) {
        case (1, Seq(Chip("hydrogen"), Chip("lithium"))) =>
      }

      assertMatch(parseFloor("The second floor contains a hydrogen generator.")) { case (2, Seq(Gen("hydrogen"))) =>
      }

      assertMatch(parseFloor("The fourth floor contains nothing relevant.")) { case (4, Seq()) =>
      }

      assertMatch(
        parseFloor(
          "The third floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator."
        )
      ) { case (3, Seq(Gen("thulium"), Chip("thulium"), Gen("plutonium"), Gen("strontium"))) => }
    }
  }

}
