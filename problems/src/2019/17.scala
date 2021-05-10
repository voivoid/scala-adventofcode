package adventOfCode.problems
package year2019

object problem17 extends baseProblem {

  import adventOfCode.utils.intcode._
  import adventOfCode.utils.geo.Point
  import adventOfCode.utils.path.{neighbours4, Turn, Bearing, doTurn, bearingLocDelta}

  override def solve1(input: Input): Int = {
    val grid = makeGrid(input.mkString)
    countIntersections(grid)
  }

  override def solve2(input: Input): Int = {
    val inputStr = input.mkString
    val grid = makeGrid(inputStr)
    val path = findPath(grid)

    val (mainRoutine, (aFunc, bFunc, cFunc)) = compressPath(path)

    val patchedMemory = parseMemory(inputStr).updated(0, 2L)
    val machineInput = String.format(s"${mainRoutine}\n${aFunc}\n${bFunc}\n${cFunc}\nn\n")

    val output = run(patchedMemory, machineInput.iterator.map(_.toLong).toList).output
    output.head.toInt
  }

  private type Grid = Array[String]
  private type Coord = Point[Int]
  private[problems] case class Cmd(turn: Turn.Turn, steps: Int)

  private[problems] def countIntersections(grid: Grid): Int = {
    val intersections = gridCoords(grid).filter(isIntersection(_, grid))
    intersections.map(coord => coord.x * coord.y).sum
  }

  private[problems] def findPath(grid: Array[String]): List[Cmd] = {
    val (robotCoord, robotBearing) = findRobot(grid)

    def nextTurn(coord: Coord, bearing: Bearing.Bearing): Option[Turn.Turn] = {
      val leftTurnLoc = coord + bearingLocDelta(doTurn(bearing, Turn.Left))
      val rightTurnLoc = coord + bearingLocDelta(doTurn(bearing, Turn.Right))

      val leftView = getCameraView(leftTurnLoc, grid)
      val rightView = getCameraView(rightTurnLoc, grid)

      if (leftView == Scaffold) { assert(rightView != Scaffold); Some(Turn.Left) }
      else if (rightView == Scaffold) { assert(leftView != Scaffold); Some(Turn.Right) }
      else None
    }

    def calcSteps(coord: Coord, bearing: Bearing.Bearing): (Int, Coord) = {
      val steps = Iterator.from(1).find(n => getCameraView(coord + bearingLocDelta(bearing, n), grid) != Scaffold).get - 1
      val nextCoord = coord + bearingLocDelta(bearing, steps)

      (steps, nextCoord)
    }

    List.unfold((robotCoord, robotBearing)) {
      case (coord, bearing) => {
        val nextTurnOpt = nextTurn(coord, bearing)
        nextTurnOpt.map(turn => {
          val nextBearing = doTurn(bearing, turn)
          val (steps, nextCoord) = calcSteps(coord, nextBearing)
          val cmd = Cmd(turn, steps)
          val nextState = (nextCoord, nextBearing)
          (cmd, nextState)
        })
      }
    }
  }

  private def cmdsToStr(commands: List[Cmd]): String = {
    commands
      .map(cmd => {
        val turnStr = cmd.turn match {
          case Turn.Left  => "L"
          case Turn.Right => "R"
          case _          => sys.error("unexpected")
        }
        turnStr + "," + cmd.steps
      })
      .mkString(",")
  }

  private def findSub(cmdGroups: List[List[Cmd]], maxGroupStrLen: Int, subgroupsLeft: Int): List[List[Cmd]] = {
    if (cmdGroups.isEmpty) List(Nil)
    else if (subgroupsLeft == 0) Nil
    else {
      val maxSubgroups = maxGroupStrLen / 3 // a minimum group length is 3 chars - "L,1"
      val firstGroup = cmdGroups.head
      val validSubgroups = firstGroup.inits.filter(sub => sub.size <= maxSubgroups && cmdsToStr(sub).size <= maxGroupStrLen && sub.nonEmpty)

      validSubgroups
        .map(sub => {
          val restGroups = cmdGroups.flatMap(group => splitBy(group, sub).filter(_.nonEmpty))
          (sub, findSub(restGroups, maxGroupStrLen, subgroupsLeft - 1))
        })
        .collectFirst {
          case (sub, rest) if rest.nonEmpty => sub :: rest
        }
        .getOrElse(Nil)
    }
  }

  private def compressPath(commands: List[Cmd]): (String, (String, String, String)) = {
    val (aCmds, bCmds, cCmds) = findSub(List(commands), 20, 3) match {
      case List(a, b, c, Nil) => (a, b, c)
      case _                  => sys.error("no solution")
    }

    val aStr = cmdsToStr(aCmds)
    val bStr = cmdsToStr(bCmds)
    val cStr = cmdsToStr(cCmds)

    val cmdsStr = cmdsToStr(commands)
    val mainStr = cmdsStr
      .replaceAll(aStr, "A")
      .replaceAll(bStr, "B")
      .replaceAll(cStr, "C")

    (mainStr, (aStr, bStr, cStr))
  }

  private[problems] def makeGrid(inputStr: String): Grid = {
    val outputStr = run(parseMemory(inputStr)).output.reverse.map(_.toChar).mkString
    outputStr.split('\n').filter(!_.isEmpty)
  }

  private def findRobot(grid: Grid): (Coord, Bearing.Bearing) = {
    val coords = gridCoords(grid)

    val robotCoord = coords.find(c => RobotChars.contains(getCameraView(c, grid))).get
    val robotBearing = getCameraView(robotCoord, grid) match {
      case '<' => Bearing.West
      case '>' => Bearing.East
      case '^' => Bearing.North
      case 'v' => Bearing.South
    }

    (robotCoord, robotBearing)
  }

  private def gridCoords(grid: Grid): Iterator[Coord] = {
    val width = grid.head.size
    val height = grid.size

    for {
      y <- (0 until height).iterator
      x <- (0 until width).iterator
    } yield Point(x, y)
  }

  private def isIntersection(coord: Coord, grid: Grid): Boolean = {
    if (getCameraView(coord, grid) != Scaffold) false
    else {
      val neighbours = neighbours4(coord)
      neighbours.forall(getCameraView(_, grid) == Scaffold)
    }
  }

  private def getCameraView(coord: Coord, grid: Grid): Char = {
    if (coord.x < 0 || coord.y < 0 || coord.y >= grid.size || coord.x >= grid.head.size) Empty
    else {
      grid(coord.y)(coord.x)
    }
  }

  private def splitBy[A](list: List[A], separator: List[A]): List[List[A]] = {
    List.unfold(list) {
      case Nil => None
      case l =>
        l.indexOfSlice(separator) match {
          case -1 => Some((l, Nil))
          case i => {
            val (left, right) = l.splitAt(i)
            Some((left, right.drop(separator.size)))
          }
        }
    }
  }

  private val Scaffold = '#'
  private val Empty = '.'
  private val RobotChars = List('v', '<', '>', '^')

}
