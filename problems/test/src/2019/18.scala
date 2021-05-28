package adventOfCode.problems.tests

object year2019_18 extends BaseTests {

  import utest._
  import adventOfCode.problems.year2019.{problem18 => problem}

  def input1 = """#########
                 |#b.A.@.a#
                 |#########""".stripMargin

  def input2 = """########################
                 |#f.D.E.e.C.b.A.@.a.B.c.#
                 |######################.#
                 |#d.....................#
                 |########################""".stripMargin

  def input3 = """########################
                 |#...............b.C.D.f#
                 |#.######################
                 |#.....@.a.B.c.d.A.e.F.g#
                 |########################""".stripMargin

  def input4 = """#################
                 |#i.G..c...e..H.p#
                 |########.########
                 |#j.A..b...f..D.o#
                 |########@########
                 |#k.E..a...g..B.n#
                 |########.########
                 |#l.F..d...h..C.m#
                 |#################""".stripMargin

  def input5 = """########################
                 |#@..............ac.GI.b#
                 |###d#e#f################
                 |###A#B#C################
                 |###g#h#i################
                 |########################""".stripMargin

  def input6 = """#######
                 |#a.#Cd#
                 |##...##
                 |##.@.##
                 |##...##
                 |#cB#Ab#
                 |#######""".stripMargin

  def input7 = """###############
                 |#d.ABC.#.....a#
                 |######...######
                 |######.@.######
                 |######...######
                 |#b.....#.....c#
                 |###############""".stripMargin

  def input8 = """#############
                 |#g#f.D#..h#l#
                 |#F###e#E###.#
                 |#dCba...BcIJ#
                 |#####.@.#####
                 |#nK.L...G...#
                 |#M###N#H###.#
                 |#o#m..#i#jk.#
                 |#############""".stripMargin

  val tests = Tests {
    test("solve1 base cases") {
      problem.solve1(input1) ==> 8
      problem.solve1(input2) ==> 86
      problem.solve1(input3) ==> 132
      //problem.solve1(input4) ==> 136 // optimize performance
      problem.solve1(input5) ==> 81
    }

    test("solve2 base cases") {
      problem.solve2(input6) ==> 8
      problem.solve2(input7) ==> 24
      problem.solve2(input8) ==> 72
    }
  }
}
