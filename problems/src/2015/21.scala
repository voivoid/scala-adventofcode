package adventOfCode.problems
package year2015

object problem21 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, Ordering[Int], playerShouldWin = true)
  }

  override def solve2(input: Input): Int = {
    solve(input, Ordering[Int].reverse, playerShouldWin = false)
  }

  private def solve(input: Input, outfitCostOrdering: Ordering[Int], playerShouldWin: Boolean): Int = {
    val boss = parseBoss(input)
    val outfits = generateOutfits().sortBy(calcOutfitCost)(outfitCostOrdering)

    val resultOutfit = outfits.find(playerShouldWin == checkIfPlayerWinsTheGame(_, boss)).getOrElse(sys.error("no solution"))
    calcOutfitCost(resultOutfit)
  }

  private def checkIfPlayerWinsTheGame(outfit: Outfit, boss: Fighter): Boolean = {
    val player = wearOutfit(outfit)

    val (fighter1, fighter2) = Iterator.iterate((player, boss))(runTurn).find(isFightOver).get
    val winnerName = if (fighter1.hitpoints > 0) fighter1.name else fighter2.name

    winnerName == PlayerName
  }

  private def isFightOver(fighters: (Fighter, Fighter)): Boolean = {
    fighters._1.hitpoints <= 0 || fighters._2.hitpoints <= 0
  }

  private def runTurn(figthers: (Fighter, Fighter)): (Fighter, Fighter) = {
    val (attacker, defender) = figthers

    val hpDamage = 1 max (attacker.damage - defender.armor)

    val nextAttacker = defender.copy(hitpoints = defender.hitpoints - hpDamage)
    val nextDefender = attacker

    (nextAttacker, nextDefender)
  }

  private case class Fighter(name: String, hitpoints: Int, damage: Int, armor: Int)
  private case class Item(name: String, cost: Int, damage: Int, armor: Int)

  private def parseBoss(input: Input): Fighter = {
    val values = input
      .getLines()
      .map(line => {
        val Array(_, value) = line.split(": ")
        value.toInt
      })
      .toList

    val List(hitpoints, damage, armor) = values: @unchecked
    Fighter(BossName, hitpoints, damage, armor)
  }

  private def BossName = "BOSS"
  private def PlayerName = "PLAYER"

  // format off
  private def weaponList = List(
    Item("Dagger", 8, 4, 0),
    Item("Shortsword", 10, 5, 0),
    Item("Warhammer", 25, 6, 0),
    Item("Longsword", 40, 7, 0),
    Item("Greataxe", 74, 8, 0)
  )

  private def armorList = List(
    Item("NoArmor", 0, 0, 0),
    Item("Leather", 13, 0, 1),
    Item("Chainmail", 31, 0, 2),
    Item("Splintmail", 53, 0, 3),
    Item("Bandedmail", 75, 0, 4),
    Item("Platemail", 102, 0, 5)
  )

  private def ringsList = Set(
    Item("NoRing1", 0, 0, 0),
    Item("NoRing2", 0, 0, 0),
    Item("Damage +1", 25, 1, 0),
    Item("Damage +2", 50, 2, 0),
    Item("Damage +3", 100, 3, 0),
    Item("Defense +1", 20, 0, 1),
    Item("Defense +2", 40, 0, 2),
    Item("Defense +3", 80, 0, 3)
  )
  // format on

  private type Outfit = List[Item]
  private def generateOutfits(): List[Outfit] = {
    for {
      weapon <- weaponList
      armor <- armorList
      ring1 <- ringsList
      ring2 <- ringsList - ring1
    } yield List(weapon, armor, ring1, ring2)
  }

  private def calcOutfitCost(outfit: Outfit): Int = {
    outfit.iterator.map(_.cost).sum
  }

  private def wearOutfit(outfit: Outfit): Fighter = {
    val damage = outfit.iterator.map(_.damage).sum
    val armor = outfit.iterator.map(_.armor).sum
    Fighter(PlayerName, 100, damage, armor)
  }

}
