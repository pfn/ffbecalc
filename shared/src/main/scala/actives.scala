package com.ffbecalc

object ActiveUtils {
  def turns(ts: Int) = ts match {
    case 1 => "one turn"
    case x => s"$x turns"
  }

  def collate(xs: List[(Int,String)],
    fmt: String,
    delim: String = ", ",
    delim2: String = "/",
    filter: ((Int,String)) => Boolean = _._1 != 0) = {
    xs.filter(filter).groupBy(_._1).toList.map { case (k,v) =>
      fmt format (v.map(_._2).mkString("/"), k)
    }.mkString(delim)
  }

  def join(xs: List[String], end: String, includeEnd: Boolean = false): String =
    xs match {
      case List(x) => if (includeEnd) end + x else x
      case x :: tail => x + ", " + join(tail, end, true)
      case Nil => ""
    }
  def or(xs: List[String]) = join(xs, " or ")
  def and(xs: List[String]) = join(xs, " and ")
}
case class UnitStats(atk: Int, defs: Int, mag: Int, spr: Int, l: Int, r: Int, variance: WeaponVariance, level: Int, elements: Set[Int], killers: Map[Int,(Int,Int)]) {
  def dw = r != 0 && l != 0
}
case class TargetStats(defs: Int, spr: Int, defBreak: Int, sprBreak: Int, tribes: Set[Int], resists: ElementResist)
case class BattleStats(unit: UnitStats, target: TargetStats)

case class ActiveData(element: List[String], tpe: String, frames: List[List[Int]], eframes: List[List[Int]], dmgsplit: List[List[Int]], atks: List[Int], atktpe: String, movetpe: Int, motiontpe: Int)
sealed trait RelatedSkill {
  def related: List[Int]
}
object ActiveData {
  def empty = ActiveData(Nil, "------", Nil, Nil, Nil, Nil, "None", 0, 0)
}
sealed trait Target
sealed trait TargetClass
sealed trait HasActiveData {
  val data: ActiveData
}
object Target {
  // 1st arg, targeting flag, 0 = self, 1 = ST, 2 = AOE, 3 = random
  // 2nd arg, target type, 1 = enemy, 2 = allies, 3 = self, 4 = all, 5 = allies no-self, 6 = ko'd, enemy-reaper

  case object Self extends Target with TargetClass
  case object Single extends Target
  case object AoE extends Target
  case object Random extends Target

  case object Enemy extends TargetClass
  case object Party extends TargetClass
  case object All extends TargetClass
  case object Allies extends TargetClass // not-self
  case object KOd extends TargetClass
}
case class SkillTarget(tgt: Target, cls: TargetClass) {
  override lazy val toString = {
    import Target._
    cls match {
      case Enemy  => tgt match {
        case Self   => "the enemy is yourself"
        case Single => "an enemy"
        case AoE    => "all enemies"
        case Random => "a random enemy"
      }
      case Party  => tgt match {
        case Self   => "self"
        case Single => "an ally"
        case AoE    => "all allies"
        case Random => "a random ally"
      }
      case All    => tgt match {
        case Self   => "all selves"
        case Single => "a target"
        case AoE    => "all"
        case Random => "a random target"
      }
      case Allies => tgt match {
        case Self   => "Self excluding Self wut"
        case Single => "an ally, excluding self"
        case AoE    => "all allies, excluding self"
        case Random => "a random ally, excluding self"
      }
      case KOd    => tgt match {
        case Self => "WTF self KO"
        case Single => "a KOd target"
        case AoE    => "all KOd targets"
        case Random => "random KOd target"
      }
      case Self   => tgt match {
        case Self   => "self"
        case Single => "self"
        case AoE    => "all selves"
        case Random => "random selves"
      }
    }
  }
}
sealed trait Condition
sealed trait ActiveEffect {
  def target: SkillTarget
}
sealed trait NoTarget {
  def target = SkillTarget(Target.Self, Target.Self)
}
object SkillTarget {
  def apply(x: Int, y: Int): SkillTarget = {
    val t = x match {
      case 0 => Target.Self
      case 1 => Target.Single
      case 2 => Target.AoE
      case 3 => Target.Random
    }

    val c = y match {
      case 0 => Target.Self
      case 1 => Target.Enemy
      case 2 => Target.Party
      case 3 => Target.Self
      case 4 => Target.All
      case 5 => Target.Allies
      case 6 => Target.KOd
    }

    SkillTarget(t, c)
  }
}
case object UnknownActiveEffect extends ActiveEffect {
  val target = SkillTarget(Target.Self, Target.Self)
}
trait ConditionalEffect
trait ConditionalBuff

sealed trait DamageResult {
  def total: Int
}
object DamageResult {
  def apply(phys: Int, mag: Int) = SingleDamage(phys, mag)
}
case class MultiDamage(damages: List[SingleDamage]) extends DamageResult {
  def total = damages.map(x => x.physical + x.magical).sum
}
case class SingleDamage(physical: Int, magical: Int) extends DamageResult {
  def total = physical + magical
}
sealed trait Damage { self: HasActiveData =>
  def ratio: Int
  def isMagic: Boolean = self.data.tpe == "Magic"
  def canDW: Boolean = self.data.atktpe == "Physical"

  def calcKillers(stats: BattleStats, sel: ((Int,Int)) => Int): Int = {
    val ts = stats.target.tribes.size
    val ks = stats.target.tribes.map(t => sel(stats.unit.killers.getOrElse(t, (0,0)))).sum
    ks / ts
  }

  def calcElements(stats: BattleStats, useEquip: Boolean): Int = {
    val skilles = data.element.toSet.map(SkillEffect.ELEMENTS)
    val es = if (useEquip) skilles ++ stats.unit.elements else skilles
    val est = es.map(e =>
      stats.target.resists.asMap.getOrElse(e, 0)).sum
    if (es.isEmpty) 0 else est / es.size
  }

  def physical(
    atk:        Int,
    ratio:      Double,
    killer:     Double  = 0,
    elemental:  Double  = 0,
    elemental2: Double  = 0,
    defs:       Int     = 25,
    itd:        Double  = 0.0,
    dw:         Boolean = true,
    level:      Int     = 100,
    l:          Int     = 0,
    r:          Int     = 0): Int = {
    if (dw) {
      physical(atk - l,
        ratio, killer, elemental, 0, defs, itd, false, level, 0) +
          physical(atk - r,
            ratio, killer, elemental + elemental2, 0, defs, itd, false, level, 0)
    } else {
      (math.floor(math.pow(atk - l, 2) / (defs * (1 - itd))).toInt *
        (1 + killer) * math.max(0, (1 + elemental)) *
          (1 + (level / 100.0)) * ratio).toInt
    }
  }
  case class Hybrid(physical: Int, magical: Int, hybrid: Int)
  def hybrid(
    atk:        Int,
    mag:        Int,
    ratio:      Double,
    killer:     Double  = 0,
    elemental:  Double  = 0,
    elemental2: Double  = 0,
    defs:       Int     = 25,
    spr:        Int     = 25,
    itd:        Double  = 0,
    its:        Double  = 0,
    dw:         Boolean = true,
    level:      Int     = 100,
    l:          Int     = 0,
    r:          Int     = 0): Hybrid = {
    if (dw) {
      val Hybrid(p1, m1, h1) = hybrid(atk - l,
        mag, ratio, killer, elemental, 0, defs, spr, itd, its, false, level)

      val Hybrid(p2, m2, h2) = hybrid(atk - r,
        mag, ratio, killer, elemental + elemental2,
          0, defs, spr, itd, its, false, level)
      Hybrid(p1+p2, m1+m2, h1+h2)
    } else {
      val p = physical(atk - l,
        ratio, killer, elemental, 0, defs, itd, false, level, l, r) / 2
      val m = magical(mag, ratio, killer, elemental, spr, its, level) / 2
      Hybrid(p, m, p+m)
    }
  }

  def magical(
    mag:       Int,
    ratio:     Double,
    killer:    Double = 0,
    elemental: Double = 0,
    spr:       Int    = 25,
    its:       Double = 0.0,
    level:     Int    = 100): Int = {
    (math.floor(math.pow(mag, 2) / (spr * (1 - its))).toInt *
      (1 + killer) * math.max(0, (1 + elemental)) *
        (1 + (level / 100.0)) * ratio).toInt
  }

  def calculateDamage(stats: BattleStats): DamageResult
}

trait MagicalDamage extends Damage { this: HasActiveData =>
  def calculateDamage(stats: BattleStats) = {
    val count = if (canDW && stats.unit.dw) 2 else 1
    val x = magical(
      stats.unit.mag, ratio / 100.0,
      calcKillers(stats, _._2), calcElements(stats, false),
      stats.target.spr, 0, stats.unit.level)
    val single = SingleDamage(0, x)
    if (count == 1) single
    else MultiDamage((0 until count).map(_ => single).toList)
  }
}

trait PhysicalDamage extends Damage { this: HasActiveData =>
  def calculateDamage(stats: BattleStats): DamageResult = {
    val count = if (canDW && stats.unit.dw) 2 else 1
    val r = SingleDamage(physical(
      stats.unit.atk - stats.unit.l, ratio / 100.0,
      calcKillers(stats, _._1), calcElements(stats, true),
      stats.target.defs, dw = false,
      level = stats.unit.level), 0)
    val l = SingleDamage(physical(
      stats.unit.atk - stats.unit.r, ratio / 100.0,
      calcKillers(stats, _._1), calcElements(stats, true),
      stats.target.defs, dw = false,
      level = stats.unit.level), 0)
    if (count == 1) r
    else MultiDamage(r :: l :: Nil)
  }
}

trait HybridDamage extends Damage { this: HasActiveData =>
  def calculateDamage(stats: BattleStats): DamageResult = {
    val count = if (canDW && stats.unit.dw) 2 else 1
    val hybr = hybrid(stats.unit.atk - stats.unit.l, stats.unit.mag, ratio / 100.0,
      calcKillers(stats, _._1), calcElements(stats, true),
      0, stats.target.defs, stats.target.spr, 0, 0,
      dw = false, stats.unit.level)
    val hybl = hybrid(stats.unit.atk - stats.unit.r, stats.unit.mag, ratio / 100.0,
      calcKillers(stats, _._1), calcElements(stats, true),
      0, stats.target.defs, stats.target.spr, 0, 0,
      dw = false, stats.unit.level)
      if (count == 1) DamageResult(hybr.physical, hybr.magical)
      else MultiDamage(
        DamageResult(hybr.physical, hybr.magical) ::
        DamageResult(hybl.physical, hybl.magical) :: Nil)
  }
}

sealed trait Healing {
  def stat: String
  def ratio: Int
  def base: Int
  def turns: Int

  def turnHeal(stats: UnitStats): (Int,Int) = {
    val (min, max) = totalHeal(stats)
    (min / turns, max / turns)
  }

  def totalHeal(stats: UnitStats): (Int, Int) = {
    val healing = ((ratio / 100.0) *
      (stats.spr * 0.5 + stats.mag * 0.1) + base).toInt

    ((0.85 * healing).toInt, healing)
  }
}

trait HPHealing extends Healing {
  def stat = "HP"
}

trait MPHealing extends Healing {
  def stat = "MP"
}

case class HexDebuffAttackEffect(target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class InvokeSkillEffect(skill: Int) extends ActiveEffect with NoTarget
case class EntrustEffect(target: SkillTarget) extends ActiveEffect
case class EsperFillEffect(min: Int, max: Int, target: SkillTarget) extends ActiveEffect
case class HealChanceEffect(ratio: Int, base: Int, chance: Int, target: SkillTarget) extends ActiveEffect with HPHealing { def turns = 1 }
case class MPHealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect with MPHealing
case class SingingHealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect with HPHealing
case class SingingMPHealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect with MPHealing
case class HealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect with HPHealing
case class FixedDamageEffect(damage: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class HybridEffect(pratio: Int, mratio: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData with HybridDamage {
  // hacky workaround, but everything thus far has been equal atk/mag hybrid
  def ratio = (pratio + mratio) / 2
}
case class DelayDamageEffect(ratio: Int, delay: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData with PhysicalDamage
case class JumpDamageEffect(ratio: Int, delay: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData with PhysicalDamage
case class MPDrainEffect(ratio: Int, drain: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class HPDrainEffect(ratio: Int, drain: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class SacrificeSelfRestoreEffect(hppct: Int, mppct: Int, target: SkillTarget) extends ActiveEffect
case class SacrificeHPPercentDamageEffect(sacrifice: Int, damage: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class SacrificeHPDamageEffect(ratio: Int, sacrifice: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData {
  def s = if (data.atktpe == "None") "*" else ""
  override lazy val toString = {
    f"Sacrifice $sacrifice%s%% HP to deal physical$s damage (${ratio.toDouble / 100}%.2fx ATK) to $target"
  }
}
// skill -> chance
case class RandomActiveEffect(skills: List[(Int,Int)], target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData with RelatedSkill {
  def related = skills.map(_._1)
}
case class RandomMagicEffect(skills: List[(Int,Int)]) extends ActiveEffect with NoTarget with RelatedSkill {
  def related = skills.map(_._1)
}
case class RestoreEffect(hp: Int, mp: Int, target: SkillTarget) extends ActiveEffect
case class SetHPEffect(hp: Int, target: SkillTarget) extends ActiveEffect
case class ReraiseEffect(pct: Int, turns: Int, target: SkillTarget) extends ActiveEffect {
  override lazy val toString = s"Auto-revive ($pct% HP) for ${ActiveUtils.turns(turns)} to $target"
}
case class SalveEffect(items: List[Int], target: SkillTarget) extends ActiveEffect
case class RestorePercentEffect(hp: Int, mp: Int, target: SkillTarget) extends ActiveEffect
case class ReducePhysicalDamageEffect(pct: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class DodgePhysicalEffect(count: Int, turns: Int, target: SkillTarget) extends ActiveEffect {
  override lazy val toString = s"Dodge $count physical attacks for ${ActiveUtils.turns(turns)} to $target"
}
case class SkipTurnsEffect(turns: Int, target: SkillTarget) extends ActiveEffect
case object DualBlackMagicEffect extends ActiveEffect with NoTarget
case class LibraEffect(target: SkillTarget) extends ActiveEffect
case object DualCastEffect extends ActiveEffect with NoTarget
case class DualMagicEffect(white: Boolean, green: Boolean, whiteCount: Int, greenCount: Int) extends ActiveEffect with NoTarget
case class HideEffect(min: Int, max: Int, target: SkillTarget) extends ActiveEffect {
  def turns = {
    if (min == max)
      ActiveUtils.turns(min)
    else s"$min to $max turns"
  }

  override lazy val toString = s"Remove caster from battle for $turns"
}
case object EscapeEffect extends ActiveEffect with NoTarget
case object ThrowEffect extends ActiveEffect with NoTarget
case object DrinkEffect extends ActiveEffect with NoTarget
case class ReduceDamageEffect(pct: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class ReduceMagicalDamageEffect(pct: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class CriticalAttackEffect(ratio: Int, miss: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class RepeatAttackEffect(ratio: Int, min: Int, max: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class MagStoreEffect(stack: Int, max: Int) extends ActiveEffect with NoTarget
case class StoreAttackEffect(stack: Int, max: Int, selfdamage: Int, target: SkillTarget) extends ActiveEffect
case class PercentHPDamageEffect(min: Int, max: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class MPDamageEffect(ratio: Int, max: Int, scaling: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class SprDamageEffect(ratio: Int, max: Int, scaling: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class DefDamageEffect(ratio: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class PhysicalEffect(ratio: Int, itd: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData with PhysicalDamage {
  def s = if (data.atktpe == "None") "*" else ""
  def realRatio = (ratio / 100.0) / (1.0 - itd / 100.0)
  override lazy val toString = f"""Physical$s ${data.element.mkString("/")} damage ($realRatio%.2fx ATK) to $target"""
}
case class PhysicalKillerEffect(ratio: Int, tribe: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class MagicalKillerEffect(ratio: Int, tribe: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class StopEffect(chance: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class StealEffect(bonus: Int, target: SkillTarget) extends ActiveEffect
case class InstantKOEffect(chance: Int, target: SkillTarget) extends ActiveEffect {
  override lazy val toString = s"Instant KO ($chance%) to $target"
}
case class ElementResistEffect(fire: Int, ice: Int, lightning: Int, water: Int,
  wind: Int, earth: Int, light: Int, dark: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class EsunaEffect(poison: Boolean, blind: Boolean, sleep: Boolean, silence: Boolean, paralyze: Boolean, confusion: Boolean, disease: Boolean, petrify: Boolean, target: SkillTarget) extends ActiveEffect
case class StatusAilmentEffect(poison: Int, blind: Int, sleep: Int, silence: Int, paralyze: Int, confusion: Int, disease: Int, petrify: Int, target: SkillTarget) extends ActiveEffect {
  def inflictString = ActiveUtils.collate(
    List(poison -> "Poison", blind -> "Blind", sleep -> "Sleep",
      silence -> "Silence", paralyze -> "Paralyze",
      confusion -> "Confusion", disease -> "Disease", petrify -> "Petrify"
    ), "%s (%s%%)")
  override lazy val toString = s"Inflict $inflictString on $target"
}
case class RandomAilmentEffect(poison: Int, blind: Int, sleep: Int, silence: Int, paralyze: Int, confusion: Int, disease: Int, petrify: Int, count: Int, target: SkillTarget) extends ActiveEffect
case class AilmentResistEffect(poison: Int, blind: Int, sleep: Int, silence: Int, paralyze: Int, confusion: Int, disease: Int, petrify: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class StackingPhysicalEffect(first: Int, stack: Int, max: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class StackingMagicalEffect(first: Int, stack: Int, max: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class MagicalEffect(ratio: Int, its: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData with MagicalDamage {
  def s = if (data.atktpe == "None") "*" else ""
  def realRatio = (ratio / 100.0) / (1.0 - its / 100.0)
  override lazy val toString = f"""Magical$s ${data.element.mkString("/")} damage ($realRatio%.2fx MAG) to $target"""
}
case class SingingBuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class BuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, target: SkillTarget) extends ActiveEffect {
  lazy val buffString = ActiveUtils.collate(List(
    atk  -> "ATK",
    defs -> "DEF",
    mag  -> "MAG",
    spr  -> "SPR",
  ), "%s by %s%%")
  override lazy val toString = {
    s"Increase $buffString for ${ActiveUtils.turns(turns)} to $target"
  }
}
case class RaiseEffect(pct: Int, target: SkillTarget) extends ActiveEffect
case class DebuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, target: SkillTarget) extends ActiveEffect {
  lazy val buffString = ActiveUtils.collate(List(
    atk  -> "ATK",
    defs -> "DEF",
    mag  -> "MAG",
    spr  -> "SPR",
  ), "%s by %s%%")
  override lazy val toString = {
    s"Decrease $buffString for ${ActiveUtils.turns(turns)} to $target"
  }
}
case class DebuffResistEffect(atk: Int, defs: Int, mag: Int, spr: Int, stop: Int, charm: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class DebuffRemoveEffect(atk: Int, defs: Int, mag: Int, spr: Int, stop: Int, charm: Int, target: SkillTarget) extends ActiveEffect
case class DispelEffect(target: SkillTarget) extends ActiveEffect
case class CharmEffect(chance: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class ProvokeEffect(chance: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class LBRateEffect(pct: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class LBFillEffect(min: Int, max: Int, target: SkillTarget) extends ActiveEffect
case class LBFillPercentEffect(pct: Int, target: SkillTarget) extends ActiveEffect
case class GiantEffect(hp: Int, mp: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class BerserkEffect(turns: Int, atk: Int, target: SkillTarget) extends ActiveEffect
case class StealGilEffect(min: Int, max: Int, target: SkillTarget) extends ActiveEffect
case class ReflectEffect(turns: Int, spells: Int, target: SkillTarget) extends ActiveEffect
case class SealingBladeEffect(turns: Int, spells: Int) extends ActiveEffect with NoTarget
case class ImbuePKillerEffect(tribe: Int, killer: Int, target: SkillTarget) extends ActiveEffect
case class ImbueMKillerEffect(tribe: Int, killer: Int, target: SkillTarget) extends ActiveEffect
case class ImbueElementEffect(fire: Int, ice: Int, lightning: Int, water: Int,
  wind: Int, earth: Int, light: Int, dark: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class PhysicalTargetCoverEffect(chance: Int, preduction: Int, mreduction: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class ActiveCounterEffect(chance: Int, stat: Int, ratio: Int, turns: Int, max: Int, target: SkillTarget) extends ActiveEffect {
  override lazy val toString = f"Grant $chance%% chance to counter physical attacks (${ratio.toDouble / 100}%.2fx ATK) to $target for ${ActiveUtils.turns(turns)}"
}
case class PhysicalAllCoverEffect(chance: Int, preduction: Int, mreduction: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class UnlockSkillEffect(skill: Int, turns: Int) extends ActiveEffect with NoTarget with RelatedSkill {
  def related = List(skill)
}
case class MultiAbilityEffect(skills: List[Int], count: Int) extends ActiveEffect with NoTarget with RelatedSkill {
  def related = skills
}
case class UnlockMultiSkillEffect(skills: List[Int], count: Int, turns: Int) extends ActiveEffect with NoTarget with RelatedSkill {
  def related = skills
}
case class ConditionalSkillEffect(trigger: List[Int], ifTrue: Int, ifFalse: Int) extends ActiveEffect with NoTarget with RelatedSkill {
  def related = List(ifTrue, ifFalse)
}
case class UnlockSkillCountedEffect(skills: List[Int], turns: Int, uses: Int, target: SkillTarget) extends ActiveEffect with RelatedSkill {
  def related = skills
}
case class DamageOrDeathEffect(ratio: Int, chance: Int, death: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class DeathImmunityEffect(turns: Int, target: SkillTarget) extends ActiveEffect
case class EvokeDamageEffect(magRatio: Int, sprRatio: Int, splits: List[Int], target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case object SurvivorFlaskEffect extends ActiveEffect with NoTarget
case object TwistOfFateEffect extends ActiveEffect with NoTarget
case object LoseHPEffect extends ActiveEffect with NoTarget
case object AddGlowEffect extends ActiveEffect with NoTarget

