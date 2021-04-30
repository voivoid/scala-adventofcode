package adventOfCode.problems
package year2015

object problem22 extends baseProblem {

  override def solve1(input: Input): Int = {
    solve(input, hardMode = false)
  }

  override def solve2(input: Input): Int = {
    solve(input, hardMode = true)
  }

  private def solve(input: Input, hardMode: Boolean): Int = {
    val boss = parseBoss(input)
    val player = Player(hp = 50, armor = 0, mana = 500)
    val initialState = State(player, boss, List.empty)

    val spellLists = generateSpellLists(initialState, hardMode)
    spellLists.iterator.map(calcSpellListMana).min
  }

  private def generateSpellLists(state: State, hardMode: Boolean): List[List[Spell]] = {
    if (state.boss.isDead) List(List())
    else {
      val s0 = if (!hardMode) state else state.decPlayerHp(1)

      if (s0.player.isDead) Nil
      else {
        val s1 = s0.applyActiveSpells
        for {
          currentSpell <- s1.availableSpells
          restSpells <- {
            val s2 = s1.runPlayerTurn(currentSpell).applyActiveSpells.runBossTurn
            generateSpellLists(s2, hardMode)
          }
        } yield currentSpell :: restSpells
      }
    }
  }

  private def calcSpellListMana(spellList: List[Spell]): Int = spellList.iterator.map(_.manaCost).sum

  private[problems] case class State(player: Player, boss: Boss, activeSpells: List[ActiveSpell]) {
    def applyActiveSpells: State = {
      val s1 = activeSpells.foldLeft(this) {
        case (state, ActiveSpell(spell, duration)) => {
          if (duration == spell.duration) spell.onStart(state)
          else if (duration == 1) spell.onEnd(state)
          else spell.cast(state)
        }
      }

      s1.decActiveSpellDuration
    }

    def decActiveSpellDuration: State = {
      copy(activeSpells = activeSpells.collect {
        case ActiveSpell(spell, duration) if duration > 1 => ActiveSpell(spell, duration - 1)
      })
    }

    def availableSpells: List[Spell] = {
      Spell.list.filter(spell => spell.manaCost <= player.mana && !activeSpells.exists { case ActiveSpell(as, _) => spell == as })
    }

    def runPlayerTurn(spell: Spell): State = {
      val s1 = if (spell.duration == 0) {
        spell.cast(this)
      } else {
        addActiveSpell(spell)
      }

      s1.incPlayerMana(-spell.manaCost)
    }

    def runBossTurn: State = {
      val hpDamage = 1 max (boss.damage - player.armor)
      decPlayerHp(hpDamage)
    }

    def decPlayerHp(hp: Int): State = copy(player = player.decHp(hp))
    def incPlayerArmor(armor: Int): State = copy(player = player.incArmor(armor))
    def incPlayerMana(mana: Int): State = copy(player = player.incMana(mana))
    def decBossHp(hp: Int): State = copy(boss = boss.decHp(hp))

    def addActiveSpell(spell: Spell): State = copy(activeSpells = ActiveSpell(spell, spell.duration) :: activeSpells)
  }

  private[problems] sealed trait Spell {
    def manaCost: Int
    def duration: Int = 0

    def cast(state: State): State
    def onStart(state: State): State = cast(state)
    def onEnd(state: State): State = cast(state)
  }

  private object Spell {
    val list = List(MagicMissle, Drain, Shield, Poison, Recharge)
  }

  private[problems] object MagicMissle extends Spell {
    override def manaCost: Int = 53
    override def cast(state: State): State = state.decBossHp(4)
  }

  private[problems] object Drain extends Spell {
    override def manaCost: Int = 73
    override def cast(state: State): State = state.decBossHp(2).decPlayerHp(-2)
  }

  private[problems] object Shield extends Spell {
    override def manaCost: Int = 113
    override def duration: Int = 6
    override def cast(state: State): State = state
    override def onStart(state: State): State = state.incPlayerArmor(7)
    override def onEnd(state: State): State = state.incPlayerArmor(-7)
  }

  private[problems] object Poison extends Spell {
    override def manaCost: Int = 173
    override def duration: Int = 6
    override def cast(state: State): State = state.decBossHp(3)
  }

  private[problems] object Recharge extends Spell {
    override def manaCost: Int = 229
    override def duration: Int = 5
    override def cast(state: State): State = state.incPlayerMana(101)
  }

  private[problems] case class ActiveSpell(spell: Spell, duration: Int) {
    assert(duration > 0)
  }

  private[problems] case class Boss(hp: Int, damage: Int) {
    def isDead = hp <= 0
    def decHp(hpToDec: Int): Boss = copy(hp = hp - hpToDec)
  }

  private[problems] case class Player(hp: Int, armor: Int, mana: Int) {
    def isDead = hp <= 0

    def incArmor(armorToDec: Int): Player = copy(armor = armor + armorToDec)
    def incMana(manaToInc: Int): Player = copy(mana = mana + manaToInc)
    def decHp(hpToDec: Int): Player = copy(hp = hp - hpToDec)
  }

  private def parseBoss(input: Input): Boss = {
    val lines = input.getLines()
    val Array(_, hpStr) = lines.next().split(": ")
    val Array(_, damageStr) = lines.next().split(": ")

    Boss(hpStr.toInt, damageStr.toInt)
  }

}
