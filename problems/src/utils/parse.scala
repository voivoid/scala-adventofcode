package adventOfCode.utils
package object parse {

  import fastparse._

  def digit[_: P] = P(CharIn("0-9"))
  def num[_: P] = P(digit.repX(1).!.map(_.toInt))
  def alpha[_: P] = CharIn("a-zA-Z").!.map(_.head)

  def parseValue[Result](input: String, parser: P[_] => P[Result]): Result = {
    val parsed = parse(input, parser)

    parsed match {
      case Parsed.Success(result, parsedChars) => {
        if (parsedChars != input.size) sys.error(s"parser failed to parse the whole input")
        result
      }
      case err => sys.error(err.toString)
    }
  }

  def isParsed[Result](input: String, parser: P[_] => P[Result]): Boolean = {
    parse(input, parser).fold((_, _, _) => false, (_, parsedChars) => parsedChars == input.size)
  }

}
