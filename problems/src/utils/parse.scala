package adventOfCode.utils
package object parse {

  import fastparse._
  import SingleLineWhitespace._

  def num[_: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))
  def alpha[_: P] = CharIn("a-zA-Z").!.map(_.head)

  def parseValue[Result](input: String, parser: P[_] => P[Result]): Result = {
    val parsed = parse(input, parser)

    parsed match {
      case Parsed.Success(result, parsed) => {
        assert(parsed == input.size)
        result
      }
      case err => sys.error(err.toString)
    }
  }

}
