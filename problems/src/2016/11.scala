package adventOfCode.problems
package year2016

object problem11 extends baseProblem {

  import scala.collection.immutable.MultiSet

  override def solve1(input: Input): Int = {
    solve(parseDevicePairs(input))
  }

  override def solve2(input: Input): Int = {
    val extraDevPairs = parseDevicePairs(input) ++ List(DevPair(1, 1), DevPair(1, 1))
    solve(extraDevPairs)
  }

  private def solve(devPairs: DevPairsSet): Int = {
    val initialState = State(1, devPairs)
    findMinimalSteps(Set(initialState), Set.empty, 0).getOrElse(sys.error("no solution"))
  }

  private type Material = String
  private type FloorNum = Int

  private sealed trait Device
  private case class Chip(material: Material) extends Device
  private case class Gen(material: Material) extends Device

  // Since all device pairs are interchangeable there is no need to distinct one pair
  // from another by their material. It will allow to greatly reduce a number of possible move combinations
  // Example: the following two states are equivalent:
  // (HM@F0, HG@F1, LM@F2, LG@F2), (LM@F0, LG@F1, HM@F2, HG@F2)
  // so both of these states can be coded with the following multiset:
  // [(0, 1), (2, 2)]
  private case class DevPair(chip: FloorNum, gen: FloorNum)
  private type DevPairsSet = MultiSet[DevPair]

  private case class State(currentFloor: FloorNum, devPairs: DevPairsSet)

  private def parseDevicePairs(input: Input): DevPairsSet = {
    val floors = input.getLines().map(parseFloor).toList

    val deviceFloorPairs = floors
      .map { case (floor, devices) => devices.map(_ -> floor) }
      .reduce(_ ++ _)

    val (chips, gens) = deviceFloorPairs.partitionMap {
      case (c: Chip, floorNum) => Left((c.material, floorNum))
      case (g: Gen, floorNum)  => Right((g.material, floorNum))
    }

    val chipsMap = chips.toMap
    val gensMap = gens.toMap

    val materials = chips.iterator.map(_._1)

    val devPairs = materials.map( material => DevPair(chipsMap(material), gensMap(material)) )
    devPairs.to(MultiSet)
  }

  @scala.annotation.tailrec
  private def findMinimalSteps(statesToCheck: Set[State], visited: Set[State], step: Int): Option[Int] = {
    if (statesToCheck.isEmpty) None
    else if (statesToCheck.exists(isFinalState)) Some(step)
    else {
      val nextStates = for {
        state <- statesToCheck
        next <- generateNextStates(state)
        if !visited.contains(next)
      } yield next

      findMinimalSteps(nextStates, visited ++ statesToCheck, step + 1)
    }
  }

  private def makeNextFloorCombinations(perm: List[DevPair], CurrentFloor: Int, nextFloor: Int): List[(List[DevPair], List[DevPair])] = {

    // Transform permutation into a pair of existing dev pairs ( that will be removed from a next state )
    // and new dev pairs ( that will be added to a next state ). New dev pairs will have their floor numbers changed
    // Note that in case a devpair has both devices on the same floor it needs some special handling:
    // Consider a devpair (2,2). It has multiple outcomes: (N, 2), (2, N), (N, N); N is a next floor
    // Keep in mind that pemutation may have 2 devpairs that are both have same floor devices, like [ (2,2), (2,2) ]
    // It have 4 different outcomes. So we need to handle all the variety of possible devpair permutations.

    def updateOneDevice(d: DevPair): DevPair = d match {
      case DevPair(CurrentFloor, floor) => DevPair(nextFloor, floor)
      case DevPair(floor, CurrentFloor) => DevPair(floor, nextFloor)
      case _ => sys.error("unexpected")
    }

    perm match {
      case l @ List(DevPair(CurrentFloor, CurrentFloor)) =>
        List(
          (l, List(DevPair(nextFloor, nextFloor))),
          (l, List(DevPair(CurrentFloor, nextFloor))),
          (l, List(DevPair(nextFloor, CurrentFloor)))
        )
      case l @ List(DevPair(CurrentFloor, CurrentFloor), DevPair(CurrentFloor, CurrentFloor)) =>
        List(
          (l, List(DevPair(CurrentFloor, nextFloor), DevPair(CurrentFloor, nextFloor))),
          (l, List(DevPair(CurrentFloor, nextFloor), DevPair(nextFloor, CurrentFloor))),
          (l, List(DevPair(nextFloor, CurrentFloor), DevPair(CurrentFloor, nextFloor))),
          (l, List(DevPair(nextFloor, CurrentFloor), DevPair(nextFloor, CurrentFloor)))
        )

      case l @ List(DevPair(CurrentFloor, CurrentFloor), p2) =>
        List(
          (l, List(DevPair(CurrentFloor, nextFloor), updateOneDevice(p2))),
          (l, List(DevPair(nextFloor, CurrentFloor), updateOneDevice(p2)))
        )

      case List(p1, p2 @ DevPair(CurrentFloor, CurrentFloor)) => makeNextFloorCombinations(List(p2, p1), CurrentFloor, nextFloor)

      case l @ List(p)      => List((l, List(updateOneDevice(p))))
      case l @ List(p1, p2) => List((l, List(updateOneDevice(p1), updateOneDevice(p2))))

      case _ => sys.error("unexpected")
    }
  }

  private def genPermutations(devices: MultiSet[DevPair], currentFloor: Int, nextFloor: Int): List[(List[DevPair], List[DevPair])] = {
    def permutations(devices: List[DevPair]): List[List[DevPair]] = devices match {
      case Nil     => List()
      case x :: xs => List(List(x)) ::: xs.map(n => List(x, n)) ::: permutations(xs)
    }

    val allPermutations = permutations(devices.toList)

    assert(currentFloor != nextFloor)
    val isGoingUp = nextFloor > currentFloor
    val isGoindDown = !isGoingUp

    val optimizedPermutations =
      if (isGoingUp && allPermutations.exists(_.size > 2)) allPermutations.filter(_.size == 2)
      else if (isGoindDown && allPermutations.exists(_.size == 1)) allPermutations.filter(_.size == 1)
      else allPermutations

    optimizedPermutations.flatMap(makeNextFloorCombinations(_, currentFloor, nextFloor))
  }

  private def generateNextStates(state: State): List[State] = {
    import adventOfCode.utils.algorithms.MultiSetExclSeq

    val currentFloor = state.currentFloor
    val currentFloorDevs = state.devPairs.filter(pair => pair.chip == currentFloor || pair.gen == currentFloor)

    for {
      nextFloor <- nextFloors(state)
      (pairsToRemove, pairsToAdd) <- genPermutations(currentFloorDevs, currentFloor, nextFloor)
      nextDevPairs = (state.devPairs -- pairsToRemove) ++ pairsToAdd
      if areDevicesCompatible(nextDevPairs, currentFloor) && areDevicesCompatible(nextDevPairs, nextFloor)
    } yield State(nextFloor, nextDevPairs)
  }

  private def areDevicesCompatible(devPairsSet: DevPairsSet, floorToCheck: Int): Boolean = {
    val noGens = !devPairsSet.exists(devPair => devPair.gen == floorToCheck)
    val nonConnectedChips = devPairsSet.exists(devPair => devPair.chip == floorToCheck && devPair.gen != floorToCheck)

    noGens || !nonConnectedChips
  }

  private def isFinalState(state: State): Boolean = {
    state.currentFloor == 4 && state.devPairs.forall(pair => pair.chip == 4 && pair.gen == 4)
  }

  private def nextFloors(state: State): List[Int] = {
    state.currentFloor match {
      case 1 => List(2)
      case 4 => List(3)
      case n => List(n - 1, n + 1)
    }
  }

  private def parseFloor(s: String): (FloorNum, Seq[Device]) = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, alpha}
    import Function.const

    def first[_: P] = P("first").map(const(1))
    def second[_: P] = P("second").map(const(2))
    def third[_: P] = P("third").map(const(3))
    def fourth[_: P] = P("fourth").map(const(4))
    def floorNum[_: P] = P(first | second | third | fourth)

    def material[_: P] = P(alpha.repX(1).!)
    def chip[_: P] = P("a" ~ material ~ "-compatible" ~ "microchip").map(Chip)
    def gen[_: P] = P("a" ~ material ~ "generator").map(Gen)
    def device[_: P] = P(chip | gen)

    def separator[_: P] = P(",".? ~ "and".?)
    def empty[_: P]: P[Seq[Device]] = P("nothing" ~ "relevant").map(const(Seq.empty))
    def devices[_: P]: P[Seq[Device]] = P(empty | device.rep(min = 1, sep = separator))

    def parser[_: P] = P("The" ~ floorNum ~ "floor" ~ "contains" ~ devices ~ ".")

    parseValue(s, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

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
