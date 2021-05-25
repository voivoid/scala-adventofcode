package adventOfCode.problems
package year2017

object problem07 extends baseProblem {

  override def solve1(input: Input): String = {
    val towers = input.getLines().map(parseTower)
    findBottomTowerName(towers)
  }

  override def solve2(input: Input): Int = {
    val towers = input.getLines().map(parseTower).toList
    val bottomTowerName = findBottomTowerName(towers)
    val towersMap = makeBottomToTopTowersMap(towers)
    val bottomTower = towersMap(bottomTowerName)

    findWeightMismatch(bottomTower, towersMap).left.getOrElse(sys.error("no solution"))
  }

  private type Name = String
  private type Weight = Int
  private type MismatchedWeight = Weight
  private[problems] case class Tower(name: Name, weight: Weight, subtowers: Seq[Name], subtowersWeight: Weight)
  private type TopToBottomNamesMap = Map[Name, Name]
  private type BottomToTopTowersMap = Map[Name, Tower]

  private type SearchResult = Either[MismatchedWeight, Tower]

  // Searching for the mismatch using DFS. Recursively visit tree nodes while
  // tower's 'subtowersWeight' checks and updates are made on the way back.
  // The function returns Left[MismatchedWeight] if the mismatch weight is found.
  // In this case the recursive search will be stopped. No excessive iteration is done
  // Right[Tower] is returned otherwise. The returned tower will a 'subtowersWeight' value set
  // to the sum of it's subtower weights + tower weight

  private def findWeightMismatch(tower: Tower, towersMap: BottomToTopTowersMap): SearchResult = tower.subtowers match {
    case Nil => Right(tower)
    case subtowerNames => {
      subtowerNames
        .foldLeft[Either[MismatchedWeight, List[Tower]]](Right(List.empty)) {
          case (eitherAcc, subtowerName) => {
            for {
              subtowersAcc <- eitherAcc
              updatedSubtower <- findWeightMismatch(towersMap(subtowerName), towersMap)
            } yield updatedSubtower :: subtowersAcc
          }
        }
        .flatMap(updatedSubtowers => {
          checkWeightMismatch(updatedSubtowers) match {
            case None           => Right(updatedSubtowers)
            case Some(mismatch) => Left(mismatch)
          }
        })
        .map(updatedSubtowers => {
          val subtowersWeightSum = updatedSubtowers.map(_.subtowersWeight).sum
          tower.copy(subtowersWeight = tower.weight + subtowersWeightSum)
        })
    }
  }

  private def checkWeightMismatch(towers: Seq[Tower]): Option[MismatchedWeight] = {
    import adventOfCode.utils.algorithms.IteratorSlidingTuple
    val commonWeight = towers.iterator.map(_.subtowersWeight).sliding2.find { case (a, b) => a == b }.get._1

    towers
      .find(_.subtowersWeight != commonWeight)
      .map(towerWithMismatch => {
        val diff = commonWeight - towerWithMismatch.subtowersWeight
        towerWithMismatch.weight + diff
      })
  }

  private def findBottomTowerName(towers: IterableOnce[Tower]): Name = {
    val namesMap = makeTopToBottomNamesMap(towers)

    @scala.annotation.tailrec
    def findBottom(name: Name, topToBottomMap: TopToBottomNamesMap): Name = {
      val lowerTower = topToBottomMap.get(name)

      if (lowerTower.isDefined) {
        findBottom(lowerTower.get, topToBottomMap)
      } else {
        name
      }
    }

    findBottom(namesMap.head._1, namesMap)
  }

  private def makeTopToBottomNamesMap(towers: IterableOnce[Tower]): TopToBottomNamesMap = {
    towers.iterator.foldLeft(Map.empty: TopToBottomNamesMap) {
      case (map, Tower(name, _, subtowers, _)) if !subtowers.isEmpty => {
        map ++ subtowers.map(_ -> name)
      }
      case (map, _) => map
    }
  }

  private def makeBottomToTopTowersMap(towers: IterableOnce[Tower]): BottomToTopTowersMap = {
    towers.iterator.foldLeft(Map.empty: BottomToTopTowersMap) {
      case (map, tower @ Tower(name, _, _, _)) => {
        map + (name -> tower)
      }
    }
  }

  private[problems] def parseTower(str: String): Tower = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, num, alpha}

    def weight[_: P] = P("(" ~ num ~ ")")
    def name[_: P] = P(alpha.repX(1).!)
    def subtowers[_: P] = P("->" ~ name.rep(min = 1, sep = ",")).?.map(_.getOrElse(Seq()))

    def parser[_: P] = P(name ~ weight ~ subtowers).map { case (name, weight, subtowers) => Tower(name, weight, subtowers, weight) }

    parseValue(str, parser(_))
  }

}
