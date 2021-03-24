package adventOfCode.problems
package year2020

object problem07 extends baseProblem {

  override def solve1(input: Input): Int = {
    val bags = input.getLines().map(parseBagInfo)
    val colorMap = makeInsideColorToOutsideColorMap(bags)
    calcBagsThatContainTheBag(Set(myBag), Set.empty, colorMap)
  }

  override def solve2(input: Input): Int = {
    val bags = input.getLines().map(parseBagInfo)
    val contentMap = makeColorToContentMap(bags)

    calcBagsThatAreContainedInTheBag(myBag, contentMap)
  }

  private type Color = String
  private type ColorMap = Map[Color, Set[Color]]
  private type ContentMap = Map[Color, Seq[Content]]

  private case class Content(num: Int, color: Color)
  private case class BagInfo(color: Color, contents: Seq[Content])

  private def myBag = "shiny gold"

  private def calcBagsThatContainTheBag(colorsToVisit: Set[Color], visited: Set[Color], colorMap: ColorMap): Int = {
    if (colorsToVisit.isEmpty) visited.size - 1
    else {
      val nextColors = colorsToVisit.flatMap(colorMap.withDefaultValue(Set.empty))

      calcBagsThatContainTheBag(nextColors.diff(visited), visited.union(colorsToVisit), colorMap)
    }
  }

  private def calcBagsThatAreContainedInTheBag(myBag: Color, contentMap: ContentMap): Int = {
    val children = contentMap.getOrElse(myBag, Seq.empty)
    if( children.isEmpty ) 0
    else {
      val childrenBags = children.map { case Content(n, color) => n + n * calcBagsThatAreContainedInTheBag(color, contentMap) }
      childrenBags.sum
    }
  }

  private def makeInsideColorToOutsideColorMap(bags: Iterator[BagInfo]): ColorMap = {
    def insertBagColors(initialMap: ColorMap, bag: BagInfo): ColorMap = {
      bag.contents.foldLeft(initialMap) { case (mapAcc, Content(_, color)) =>
        mapAcc.updatedWith(color) {
          case None => Some(Set(bag.color))
          case Some(valueSet) => Some(valueSet + bag.color)
        }
      }
    }

    bags.foldLeft(Map.empty[Color, Set[Color]])(insertBagColors)
  }

  private def makeColorToContentMap(bags: Iterator[BagInfo]): ContentMap = {
    bags.foldLeft(Map.empty[Color, Seq[Content]]) {
      case (mapAcc, BagInfo(color, contents)) => mapAcc.updated(color, contents)
    }
  }

  private def parseBagInfo(str: String): BagInfo = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, alpha, num}
    import Function.const

    def color[_: P]: P[Color] = P(alpha.repX(1) ~ alpha.repX(1)).!
    def bag[_: P] = P(color ~ ("bags" | "bag"))
    def noContent[_: P] = P("no" ~ "other" ~ "bags").map(const(Seq.empty[Content]))
    def content[_: P] = P(num ~ bag).map(Content tupled _)
    def contents[_: P] = P(noContent | content.rep(min = 1, sep = ","))
    def parser[_: P] = P(bag ~ "contain" ~ contents ~ ".").map(BagInfo tupled _)

    parseValue(str, parser(_))
  }

  private[problems] def implTests(): Unit = {
    import utest._

    assertMatch(parseBagInfo("light red bags contain 1 bright white bag, 2 muted yellow bags.")) {
      case BagInfo("light red", Seq(Content(1, "bright white"), Content(2, "muted yellow"))) =>
    }

    assertMatch(parseBagInfo("faded blue bags contain no other bags.")) { case BagInfo("faded blue", Seq()) =>
    }
  }

}
