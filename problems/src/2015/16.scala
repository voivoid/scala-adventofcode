package adventOfCode.problems
package year2015

object problem16 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, predMap1)
  }

  override def solve2(input: Input): Int = {
    solve(input, predMap2)
  }

  private def solve(input: Input, paramsToCheckFor: ParamsPredMap): Int = {
    val aunts = input.getLines().map(parseAunt)
    val theAunt = aunts.find(isTheAunt(_, paramsToCheckFor)).getOrElse(sys.error("no solution"))

    theAunt.id
  }

  private def predMap1: ParamsPredMap = Map(
    Param.children -> (_ == 3),
    Param.cats -> (_ == 7),
    Param.samoyeds -> (_ == 2),
    Param.pomeranians -> (_ == 3),
    Param.akitas -> (_ == 0),
    Param.vizslas -> (_ == 0),
    Param.goldfish -> (_ == 5),
    Param.trees -> (_ == 3),
    Param.cars -> (_ == 2),
    Param.perfumes -> (_ == 1)
  )

  private def predMap2: ParamsPredMap =
    predMap1 ++ Map(Param.cats -> (_ > 7), Param.trees -> (_ > 3), Param.pomeranians -> (_ < 3), Param.goldfish -> (_ < 5))

  private def isTheAunt(aunt: Aunt, paramsToCheckFor: ParamsPredMap): Boolean = {
    aunt.paramsMap.forall { case (param, quantity) => paramsToCheckFor(param)(quantity) }
  }

  private[problems] object Param extends scala.Enumeration {
    val children, cats, samoyeds, pomeranians, akitas, vizslas, goldfish, trees, cars, perfumes = Value
    def fromString(s: String): Value = values.find(_.toString == s).getOrElse(sys.error("unexpected param name"))
    type Param = Value
  }

  private type ParamsPredMap = Map[Param.Param, Int => Boolean]

  private[problems] type ParamsMap = Map[Param.Param, Int]
  private[problems] case class Aunt(id: Int, paramsMap: ParamsMap)

  private[problems] def parseAunt(s: String): Aunt = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num, alpha}

    def paramName[_: P] = P(alpha.repX(1).!)
    def param[_: P] = P(paramName ~ ":" ~ num)
    def params[_: P] = P(param.rep(min = 1, sep = ","))

    def parser[_: P] = P("Sue" ~ num ~ ":" ~ params)

    val (parsedId, parsedParams) = parseValue(s, parser(_))

    val paramsMap = parsedParams.map { case (param, quantity) => Param.fromString(param) -> quantity }.toMap

    Aunt(parsedId, paramsMap)
  }

}
