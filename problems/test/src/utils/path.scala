package adventOfCode.utils.tests

object path extends utest.TestSuite {

  import utest._
  import adventOfCode.utils.path._
//
  val tests = Tests {
    doTurn(Bearing.North, Turn.Left) ==> Bearing.West
    doTurn(Bearing.North, Turn.Right) ==> Bearing.East
    doTurn(Bearing.West, Turn.Left) ==> Bearing.South
    doTurn(Bearing.West, Turn.Right) ==> Bearing.North
  }
}
