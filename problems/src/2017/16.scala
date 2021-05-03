package adventOfCode.problems
package year2017

object problem16 extends baseProblem {

  override def solve1(input: Input): String = {
    solve1(input, DefaultProgramsNum)
  }

  def solve1(input: Input, programsNum: Int): String = {
    solve(input, programsNum, 1)
  }

  override def solve2(input: Input): String = {
    solve(input, DefaultProgramsNum, 1000000000L)
  }

  private def solve(input: Input, programsNum: Int, dances: Long): String = {
    val moves = parseMoves(input.mkString)
    val initialState = State(makePrograms(programsNum), 0)

    import adventOfCode.utils.algorithms.{IteratorFindFirstDuplicate, IteratorLast}
    val dancesToRun =
      if (dances == 1) 1
      else {
        val cycleLen = Iterator.iterate(initialState)(runDance(moves, _)).findFirstDuplicateIndex.getOrElse(sys.error("no fast solution"))
        dances % cycleLen
      }

    val finalState = Iterator.iterate(initialState, dancesToRun.toInt + 1)(runDance(moves, _)).last

    unspinPrograms(finalState)
  }

  private type Program = Char
  private type Programs = Vector[Program]

  private def DefaultProgramsNum = 16

  private def makePrograms(programsNum: Int): Programs = {
    (0 until programsNum).map(i => ('a' + i).toChar).toVector
  }

  private def unspinPrograms(state: State): String = {
    import state._
    val shift = programs.size - offset

    (programs.drop(shift) ++ programs.take(shift)).mkString
  }

  private def runDance(moves: Seq[Move], state: State): State = {
    moves.foldLeft(state) {
      case (accState, Spin(x)) => accState.copy(offset = (accState.offset + x) % accState.programs.size)
      case (accState, Exchange(i1, i2)) => {
        import accState._
        val size = programs.size
        val oi1 = (i1 - offset + size) % size
        val oi2 = (i2 - offset + size) % size
        val p1 = programs(oi1)
        val p2 = programs(oi2)

        accState.copy(programs = accState.programs.updated(oi1, p2).updated(oi2, p1))
      }
      case (accState, Partner(p1, p2)) => {
        val i1 = accState.programs.indexOf(p1)
        val i2 = accState.programs.indexOf(p2)
        accState.copy(programs = accState.programs.updated(i1, p2).updated(i2, p1))
      }
    }
  }

  private case class State(programs: Programs, offset: Int)

  private sealed trait Move
  private case class Spin(x: Int) extends Move
  private case class Exchange(i1: Int, i2: Int) extends Move
  private case class Partner(p1: Char, p2: Char) extends Move

  private def parseMoves(str: String): Seq[Move] = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def s[_: P]: P[Spin] = P("s" ~ num).map(Spin)
    def x[_: P]: P[Exchange] = P("x" ~ num ~ "/" ~ num).map(Exchange tupled _)
    def p[_: P]: P[Partner] = P("p" ~ SingleChar ~ "/" ~ SingleChar).map(Partner tupled _)
    def move[_: P]: P[Move] = P(s | x | p)
    def parser[_: P]: P[Seq[Move]] = P(move.rep(min = 1, sep = ","))

    parseValue(str, parser(_))
  }

}
