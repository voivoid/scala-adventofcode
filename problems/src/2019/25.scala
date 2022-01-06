package adventOfCode.problems
package year2019

object problem25 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): String = {
    val initialMachine = run(parseMemory(input))

    val (machineWillAllItems, _, pathToPressureRoom) = gatherAllItems(initialMachine)

    val dirToPressureRoom :: pathToSecurityRoom = pathToPressureRoom : @unchecked

    val MachineInSecurityRoom = goToSecurityRoom(machineWillAllItems, pathToSecurityRoom.reverse)
    val outputWithPassword = findCorrectWeight(MachineInSecurityRoom, dirToPressureRoom)

    parsePassword(outputWithPassword)
  }

  override def solve2(input: Input): Int = {
    0 // the problems has no part 2
  }

  private type Item = String
  private type MoveDir = String
  private type RoomName = String
  private type Path = List[MoveDir]

  private def gatherAllItems(machine: Machine): (Machine, Set[RoomName], Path) = {

    def impl(machine: Machine, visited: Set[RoomName], returnDir: Option[MoveDir], pathSoFar: Path, pressureRoomPath: Path): (Machine, Set[RoomName], Path) = {
      val room = parseRoom(getOutputString(machine))

      if(visited.contains(room.name)) (moveDroid(machine, returnDir.get), visited, pressureRoomPath)
      else if(room.name == PressureSensitiveRoom) (machine, visited, pathSoFar)
      else {
        val machineWithItems =
          room.items.iterator.filter(!ProhibitedItems.contains(_)).foldLeft(machine)(takeItem)

        val (finalMachine, finalVisited, finalPath) = room.moves.foldLeft((machineWithItems, visited.incl(room.name), pressureRoomPath)) {
          case (acc, door) if returnDir.contains(door) => acc
          case ((accMachine, accVisited, accPath), moveDir) => {
            impl(moveDroid(accMachine, moveDir), accVisited, Some(oppositeDir(moveDir)), moveDir :: pathSoFar, accPath)
          }
        }

        (returnDir.map(b => moveDroid(finalMachine, b)).getOrElse(finalMachine), finalVisited, finalPath)
      }
    }

    impl(machine, Set.empty, None, List.empty, List.empty)
  }

  private def takeItem(machine: Machine, item: Item): Machine = {
    execCmd(machine, "take " + item)
  }

  private def dropItem(machine: Machine, item: Item): Machine = {
    execCmd(machine, "drop " + item)
  }

  private def moveDroid(machine: Machine, dir: MoveDir): Machine = execCmd(machine, dir)

  private def getInventory(machine: Machine): List[Item] = {
    parseInventory(getOutputString(execCmd(machine, "inv")))
  }

  private def execCmd(machine: Machine, cmd: String): Machine = {
    val input = (cmd + "\n").map(_.toLong).toList
    resumeMachine(resetOutput(machine), input)
  }

  private def getOutputString(machine: Machine): String = machine.output.map(_.toChar).mkString.reverse

  private def goToSecurityRoom(machine: Machine, pathToSecurityRoom: Path): Machine = {
    val machineInSecurityRoom = pathToSecurityRoom.foldLeft(machine)(moveDroid)
    assert(parseRoom(getOutputString(machineInSecurityRoom)).name == SecurityRoom)

    machineInSecurityRoom
  }

  private def findCorrectWeight(machineInSecurityRoom: Machine, pressureRoomEnterDir: MoveDir): String = {
    val inventory = getInventory(machineInSecurityRoom)

    val emptyMachine = inventory.foldLeft(machineInSecurityRoom)(dropItem)
    assert(getInventory(emptyMachine).isEmpty)

    val combinations = (1 to inventory.size).iterator.flatMap(itemsNum => inventory.combinations(itemsNum))

    combinations.iterator
      .map(combination => isItemsWeightValid(emptyMachine, combination, pressureRoomEnterDir))
      .find(_.checkPassed)
      .get.output
  }

  private case class ItemsCheckResult(output: String, checkPassed: Boolean)

  private def isItemsWeightValid(emptyMachine: Machine, items: List[Item], enterDir: MoveDir): ItemsCheckResult = {
    val m1 = items.foldLeft(emptyMachine)(takeItem)

    val m2 = moveDroid(m1, enterDir)
    val s = getOutputString(m2)
    ItemsCheckResult(s, !s.contains("Alert!"))
  }

  private def parsePassword(s: String): String = {

    /*
== Pressure-Sensitive Floor ==
Analyzing...

Doors here lead:
- east

A loud, robotic voice says "Analysis complete! You may proceed." and you enter the cockpit.
Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly.
"Oh, hello! You should be able to get in by typing 2415919488 on the keypad at the main airlock."
*/
    ???
  }

  private case class Room(name: RoomName, moves: List[MoveDir], items: List[Item])
  private def parseRoom(s: String): Room = {
    val lines = s.split('\n').filter(_.nonEmpty)
    val roomName = lines(0)
    if(roomName == PressureSensitiveRoom) Room(roomName, List.empty, List.empty)
    else {
      val iDoors = lines.indexOf("Doors here lead:") + 1;
      assert(iDoors != -1)
      val iItems = lines.indexOf("Items here:") + 1
      val iCommand = lines.indexOf("Command?") + 1;
      assert(iCommand != -1)

      val doorsNum = if (iItems != 0) iItems - iDoors - 1 else iCommand - iDoors - 1
      val itemsNum = if (iItems != 0) iCommand - iItems - 1 else 0

      val doors = parseList(lines.slice(iDoors, iDoors + doorsNum))
      val items = parseList(lines.slice(iItems, iItems + itemsNum))

      Room(roomName, doors, items)
    }
  }

  private def parseInventory(s: String): List[Item] = {
    val lines = s.split('\n').filter(_.nonEmpty)
    if(lines.head == "You aren't carrying any items.") List.empty
    else {
      val iItems = lines.indexOf("Items in your inventory:") + 1;
      assert(iItems != -1)
      val iCommand = lines.indexOf("Command?");
      assert(iCommand != -1)

      val itemsNum = iCommand - iItems
      parseList(lines.slice(iItems, iItems + itemsNum))
    }
  }

  private def parseList( list: Iterable[String] ): List[String] = {
    list.iterator.map(_.drop(2)).toList
  }

  private def ProhibitedItems = Set("photons", "infinite loop", "molten lava", "escape pod", "giant electromagnet")
  private def PressureSensitiveRoom = "== Pressure-Sensitive Floor =="
  private def SecurityRoom = "== Security Checkpoint =="

  private def oppositeDir(move: MoveDir): MoveDir = move match {
    case "north" => "south"
    case "east" => "west"
    case "west" => "east"
    case "south" => "north"
  }

}
