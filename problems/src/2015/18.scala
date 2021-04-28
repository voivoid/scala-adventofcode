package adventOfCode.problems
package year2015

object problem18 extends baseProblem {

  import adventOfCode.utils.path.neighbours8
  import adventOfCode.utils.geo.Point

  override def solve1(input: Input): Int = {
    solve1(input, 100)
  }

  def solve1(input: Input, steps: Int): Int = {
    solve(input, steps, cornerLights = false)
  }

  override def solve2(input: Input): Int = {
    solve2(input, 100)
  }

  def solve2(input: Input, steps: Int): Int = {
    solve(input, steps, cornerLights = true)
  }

  private def solve(input: Input, steps: Int, cornerLights: Boolean): Int = {
    val lines = input.getLines().buffered
    val side = lines.head.length

    val gridData = lines.mkString
    val initialGrid = {
      val grid = Array.tabulate(side, side) { case (y, x) => gridData(y * side + x) }
      if (cornerLights) setCornerLightOn(grid) else grid
    }

    import adventOfCode.utils.algorithms.IteratorLast

    val finalState = Iterator.iterate(initialGrid, steps + 1)(calcNextGen(_, cornerLights)).last
    finalState.iterator.flatten.count(_ == LightOn)
  }

  private type Grid = Array[Array[Char]]

  private def calcNextGen(grid: Grid, cornerLights: Boolean): Grid = {
    val side = grid(0).length

    val nextGenGrid = Array.tabulate(side, side) { case (y, x) =>
      calcNextState(grid, x, y)
    }

    if (!cornerLights) nextGenGrid
    else setCornerLightOn(nextGenGrid)
  }

  private def setCornerLightOn(grid: Grid): Grid = {
    val si = grid(0).length - 1

    grid(0)(0) = LightOn
    grid(0)(si) = LightOn
    grid(si)(0) = LightOn
    grid(si)(si) = LightOn

    grid
  }

  private def calcNextState(grid: Grid, x: Int, y: Int): Char = {
    val cell = grid(y)(x)
    val neighboursWithLightOn = countNeighboursOn(grid, x, y)

    if (cell == LightOn) {
      if (neighboursWithLightOn == 2 || neighboursWithLightOn == 3) LightOn else LightOff
    } else {
      if (neighboursWithLightOn == 3) LightOn else LightOff
    }
  }

  private def countNeighboursOn(grid: Grid, x: Int, y: Int): Int = {
    val side = grid(0).length

    val coord = Point(x, y)
    val neighbours = neighbours8(coord)

    neighbours.count {
      case Point(x, y) if x < 0 || y < 0 || x >= side || y >= side => false
      case Point(x, y)                                             => grid(y)(x) == LightOn
    }
  }

  private val LightOn = '#'
  private val LightOff = '.'
}
