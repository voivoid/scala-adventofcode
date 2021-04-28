package adventOfCode.problems
package year2015

object problem15 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, _ => true)
  }

  override def solve2(input: Input): Int = {
    solve(input, ingredients => calcComponentsSum(ingredients, _.calories) == 500)
  }

  private def solve(input: Input, ingredientsFilterPred: Ingredients => Boolean): Int = {
    val ingredients = input.getLines().map(parseIngredient).toList
    val teaspoonPermutations = generateTeaspoons(ingredients.size, teaspoonsAvailable = 100)
    val measuredIngredients = teaspoonPermutations.map(ingredients.zip(_))

    measuredIngredients.filter(ingredientsFilterPred).map(calcCookieScore).max
  }

  private def generateTeaspoons(ingredientsLeft: Int, teaspoonsAvailable: Int): IndexedSeq[List[Int]] = ingredientsLeft match {
    case 1 => IndexedSeq(List(teaspoonsAvailable))
    case _ => {
      for {
        teaspoons <- 1 to (teaspoonsAvailable - ingredientsLeft + 1)
        rest <- generateTeaspoons(ingredientsLeft - 1, teaspoonsAvailable - teaspoons)
      } yield teaspoons :: rest
    }
  }

  private type Ingredients = List[(Ingredient, Int)]

  private def calcComponentsSum(ingredients: Ingredients, getParam: Ingredient => Int): Int = {
    val sum = ingredients.map { case (ingredient, teaspoons) => getParam(ingredient) * teaspoons }.sum
    sum max 0
  }

  private def calcCookieScore(ingredients: Ingredients): Int = {
    val calc = calcComponentsSum(ingredients, _)
    calc(_.capacity) * calc(_.durability) * calc(_.flavor) * calc(_.texture)
  }

  private[problems] case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  private[problems] def parseIngredient(s: String): Ingredient = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, alpha, num}

    def name[_: P] = P(alpha.repX(1).!)
    def param[_: P] = P(name ~ num)
    def params[_: P] = P(param.rep(exactly = 5, sep = ",")).map(_.toMap)
    def parser[_: P] = P(name ~ ":" ~ params)

    val (parsedName, parsedParams) = parseValue(s, parser(_))

    Ingredient(
      parsedName,
      parsedParams("capacity"),
      parsedParams("durability"),
      parsedParams("flavor"),
      parsedParams("texture"),
      parsedParams("calories")
    )
  }

}
