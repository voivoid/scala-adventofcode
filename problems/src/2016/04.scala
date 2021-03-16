package adventOfCode.problems
package year2016

import adventOfCode.utils.parse.alpha

object problem04 extends baseProblem {

  override def solve1(input: Input): Int = {
    getRealRooms(input).map(_.id).sum
  }

  override def solve2(input: Input): Int = {
    val storageRoom = getRealRooms(input).find(isStorageRoom).getOrElse(sys.error("no solution"))
    storageRoom.id
  }

  private case class Room(name: String, id: Int, checksum: String)

  private def getRealRooms(input: Input): Iterator[Room] = {
    input.getLines().map(parseRoom).filter(isRealRoom)
  }

  private def isRealRoom(room: Room): Boolean = {
    val lettersOccurence = room.name.filter(_ != '-').groupBy(identity).iterator.map { case (c, str) => (str.size, c) }
    val mostOccured5Letters = lettersOccurence.toVector
      .sortWith { case ((size1, char1), (size2, char2)) =>
        if (size1 != size2) size1 >= size2 else char1 < char2
      }
      .take(5)
      .map(_._2)

    mostOccured5Letters.mkString == room.checksum
  }

  private def isStorageRoom(room: Room): Boolean = decryptRoomName(room) == "northpole object storage"

  private def decryptRoomName(room: Room): String = {
    decryptName(room.name, room.id)
  }

  private def decryptName(name: String, shift: Int): String = {
    val letters = 1 + 'z' - 'a'

    name.map {
      case '-' => ' '
      case c => {
        assert(c.isLower)
        ('a' + ((c - 'a' + shift) % letters)).toChar
      }
    }
  }

  private def parseRoom(input: String): Room = {
    import fastparse._
    import NoWhitespace._
    import adventOfCode.utils.parse.{parseValue, num}

    def word[_: P] = P(alpha.rep(1).!)
    def name[_: P] = P((word ~ ("-" ~ word).rep).!)
    def id[_: P] = P("-" ~ num)
    def checksum[_: P] = P("[" ~ P(alpha.rep(1).!) ~ "]")
    def parser[_: P] = P(name ~ id ~ checksum).map(Room tupled _)

    parseValue(input, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

    assertMatch(parseRoom("aaaaa-bbb-z-y-x-123[abxyz]")) { case Room("aaaaa-bbb-z-y-x", 123, "abxyz") => }
  }

}
