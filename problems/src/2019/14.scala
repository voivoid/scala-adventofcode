package adventOfCode.problems
package year2019

object problem14 extends baseProblem {

  override def solve1(input: Input): Long = {
    val reactionsMap = makeReactionsMap(input)
    val (_, oreUsed) = calcOre(makeFuel(1), reactionsMap, Map.empty, 0L)

    oreUsed
  }

  override def solve2(input: Input): Long = {
    val reactionsMap = makeReactionsMap(input)
    val oreCargo = 1000000000000L
    val (_, maxOrePerFuel) = calcOre(makeFuel(1), reactionsMap, Map.empty, 0L)

    calcMaxFuel(oreCargo, Map.empty, 0, maxOrePerFuel, reactionsMap)
  }

  @scala.annotation.tailrec
  private def calcMaxFuel(oreLeft: Long, storage: MaterialMap, fuelTotal: Long, maxOrePerFuel: Long, reactionsMap: ReactionsMap): Long = {
    val fuelToProduce = (oreLeft / maxOrePerFuel) max 1
    val (updatedStorage, oreUsed) = calcOre(makeFuel(fuelToProduce), reactionsMap, storage, 0)

    if (oreUsed > oreLeft) fuelTotal
    else {
      calcMaxFuel(oreLeft - oreUsed, updatedStorage, fuelTotal + fuelToProduce, maxOrePerFuel, reactionsMap)
    }

  }

  private def makeFuel(quantity: Long) = Material(quantity, "FUEL")

  private def calcOre(materialToProduce: Material, reactionsMap: ReactionsMap, storage: MaterialMap, oreUsed: Long): (MaterialMap, Long) = {
    if (materialToProduce.name == "ORE" || materialToProduce.quantity == 0) {
      (storage, oreUsed + materialToProduce.quantity)
    } else {
      val reaction = reactionsMap(materialToProduce.name)

      val storageQuantity = materialToProduce.quantity min getStoredQuantity(materialToProduce, storage)
      val reactionsToRun = roundUp(materialToProduce.quantity - storageQuantity, reaction.result.quantity)

      val storage1 = removeMaterial(storage, materialToProduce.name, storageQuantity)

      val (storage2, updatedOreUsed) =
        reaction.requirements.foldLeft((storage1, oreUsed)) {
          case ((accStorage, accOre), requirement) => {
            calcOre(requirement.copy(quantity = requirement.quantity * reactionsToRun), reactionsMap, accStorage, accOre)
          }
        }

      val excessiveQuantity = reactionsToRun * reaction.result.quantity - (materialToProduce.quantity - storageQuantity)
      val storage3 = addMaterial(storage2, materialToProduce.name, excessiveQuantity)

      (storage3, updatedOreUsed)
    }
  }

  private[problems] case class Material(quantity: Long, name: MaterialName)
  private[problems] case class Reaction(requirements: Seq[Material], result: Material)

  private type MaterialName = String
  private type ReactionsMap = Map[MaterialName, Reaction]
  private type MaterialMap = Map[MaterialName, Material]

  private def getStoredQuantity(material: Material, storageMap: MaterialMap): Long = {
    storageMap.get(material.name).map(_.quantity).getOrElse(0)
  }

  private[problems] def parseReaction(s: String): Reaction = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, numL, alpha}

    def name[_: P] = P(alpha.repX(1)).!
    def material[_: P]: P[Material] = P(numL ~ name).map(Material tupled _)

    def from[_: P] = P(material.rep(min = 1, sep = ","))
    def reaction[_: P] = P(from ~ "=>" ~ material).map(Reaction tupled _)

    parseValue(s, reaction(_))
  }

//  private def mulMaterialMap(materialMap: MaterialMap, mul: Long): MaterialMap = {
//    materialMap.map{ case (n, m@Material(q, _)) => n -> m.copy(quantity = q * mul ) }
//  }

  private def addMaterial(materialMap: MaterialMap, name: MaterialName, quantity: Long): MaterialMap = {
    if (quantity == 0) materialMap
    else
      materialMap.updatedWith(name)(value =>
        value
          .orElse(Some(Material(0, name)))
          .flatMap(m => {
            val newQuantity = m.quantity + quantity
            if (newQuantity != 0) Some(m.copy(quantity = newQuantity)) else None
          })
      )
  }

  private def removeMaterial(materialMap: MaterialMap, name: MaterialName, quantity: Long): MaterialMap = {
    addMaterial(materialMap, name, -quantity)
  }

  private def makeReactionsMap(input: Input): ReactionsMap = {
    val reactions = input.getLines().map(parseReaction)
    reactions.iterator.map(reaction => reaction.result.name -> reaction).toMap
  }

  private def roundUp(dividend: Long, divisor: Long): Long = {
    import scala.math.Integral.Implicits._
    val (quo, rem) = dividend /% divisor
    quo + (if (rem != 0) 1 else 0)
  }

}
