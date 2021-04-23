package adventOfCode.problems
package year2015

import play.api.libs.json.JsValue

object problem12 extends baseProblem {

  import play.api.libs.json.{Json, JsNumber, JsArray, JsObject, JsString}

  override def solve1(input: Input): Int = {
    sumNums(Json.parse(input.mkString))
  }

  override def solve2(input: Input): Int = {
    sumNumsNotRed(Json.parse(input.mkString))
  }

  private def sumNums(json: JsValue): Int = json match {
    case JsNumber(num) => num.intValue
    case JsArray(arr)  => arr.map(sumNums).sum
    case JsObject(obj) => obj.map { case (_, j) => sumNums(j) }.sum
    case _             => 0
  }

  private def sumNumsNotRed(json: JsValue): Int = json match {
    case JsObject(obj) => if (hasRed(obj.values)) 0 else obj.map { case (_, j) => sumNumsNotRed(j) }.sum
    case JsArray(arr)  => arr.map(sumNumsNotRed).sum
    case _             => sumNums(json)
  }

  private def hasRed(objValues: Iterable[JsValue]): Boolean = {
    objValues.exists {
      case JsString("red") => true
      case _               => false
    }
  }

}
