package adventOfCode.problems
package year2017

object problem12 extends baseProblem {

  import scala.collection.immutable.MultiDict

  override def solve1(input: Input): Int = {
    findGroupSize(program = 0, makeConnectionsDict(input))
  }

  override def solve2(input: Input): Int = {
    findGroupsNumber(makeConnectionsDict(input))
  }

  private type Program = Int
  private type ConnectionsDict = MultiDict[Program, Program]

  @scala.annotation.tailrec
  def findConnections(programsToCheck: Set[Program], visited: Set[Program], connectionsMap: ConnectionsDict): Set[Program] = {
    if (programsToCheck.isEmpty) visited
    else {
      val program = programsToCheck.head
      val connected = connectionsMap.get(program).diff(visited)
      findConnections(programsToCheck - program ++ connected, visited + program, connectionsMap)
    }
  }

  private def findGroupSize(program: Program, connectionsMap: ConnectionsDict): Int = {
    findConnections(Set(program), Set.empty, connectionsMap).size
  }

  private def findGroupsNumber(connectionsMap: ConnectionsDict): Int = {

    @scala.annotation.tailrec
    def calc(map: ConnectionsDict, acc: Int): Int = {
      if (map.isEmpty) acc
      else {
        val program = map.head._1
        val connections = findConnections(Set(program), Set.empty, map)
        calc(connections.foldLeft(map)(_.removeKey(_)), acc + 1)
      }
    }

    calc(connectionsMap, 0)
  }

  private def makeConnectionsDict(input: Input): ConnectionsDict = {
    val connections = input.getLines().map(parseConnections)

    val pairs = for {
      c <- connections
      to <- c.to
    } yield c.from -> to

    pairs.to(MultiDict)
  }

  private[problems] case class Connection(from: Int, to: Seq[Int])

  private[problems] def parseConnections(s: String): Connection = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def parser[_: P] = P(num ~ "<->" ~ num.rep(min = 1, sep = ",")).map(Connection tupled _)

    parseValue(s, parser(_))
  }

}
