package adventOfCode.problems
package year2020

object problem11 extends baseProblem {

  import adventOfCode.utils.geo.Point
  type Coord = Point[Int]

  override def solve1(input: Input): Int = {
    solve(input, isAdjacentOccupied, 4)
  }

  override def solve2(input: Input): Int = {
    solve(input, isVisibleOccupied, 5)
  }

  private def solve(input: Input, isOccupied: IsOccupiedF, occupiedSeats: Int): Int = {
    val lines = input.getLines().zipWithIndex

    val seatsMap = lines.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (c, x) => Point(x, y) -> c }
    }.toMap

    val seatStates = Iterator.iterate(seatsMap)(calcNextState(_, isOccupied, occupiedSeats))

    import adventOfCode.utils.algorithms.IteratorSlidingTuple
    val finalSeatsMap = seatStates.sliding2.find { case (s1, s2) => s1 == s2 }.get._1

    finalSeatsMap.values.count(_ == Occupied)
  }

  private type SeatState = Char
  private val Occupied: SeatState = '#'
  private val Empty: SeatState = 'L'
  private val Floor: SeatState = '.'
  private type IsOccupiedF = (Coord, Coord, SeatsMap) => Boolean

  private type SeatsMap = Map[Coord, Char]

  def calcNextState(seatsMap: SeatsMap, isOccupied: IsOccupiedF, occupiedSeats: Int): SeatsMap = {
    seatsMap.map { case (coord, seatState) =>
      val nextState = {
        import adventOfCode.utils.path.neighbours8

        seatState match {
          case Empty => {
            val neighbourDirs = neighbours8(Point(0, 0))
            val anyOccupied = neighbourDirs.exists(isOccupied(coord, _, seatsMap))
            if (anyOccupied) Empty else Occupied
          }

          case Occupied => {
            val fourOrMoreOccupied = neighbours8(Point(0, 0)).view.filter(isOccupied(coord, _, seatsMap)).sizeCompare(occupiedSeats) >= 0
            if (fourOrMoreOccupied) Empty else Occupied
          }

          case Floor => Floor
        }
      }

      coord -> nextState
    }
  }

  private def isAdjacentOccupied(coord: Coord, neighbourDir: Coord, seatsMap: SeatsMap): Boolean = {
    seatsMap.getOrElse(coord + neighbourDir, Floor) == Occupied
  }

  private def isVisibleOccupied(coord: Coord, neighbourDir: Coord, seatsMap: SeatsMap): Boolean = {
    val seatStates = Iterator
      .iterate(neighbourDir)(_ + neighbourDir)
      .map(shift => seatsMap.getOrElse(coord + shift, Empty))

    seatStates.find(_ != Floor).get == Occupied
  }

}
