package yaffbedb

// TODO handle weapons/variance?
case class UnitStats(atk: Int, defs: Int, mag: Int, spr: Int, l: Int, r: Int, dw: Boolean, level: Int, elements: Set[Int], killers: Map[Int,(Int,Int)])
case class TargetStats(defs: Int, spr: Int, tribes: Set[Int], resists: ElementResist)
case class BattleStats(unit: UnitStats, target: TargetStats)
case class Damage(physical: Int, magical: Int)

case class ActiveData(element: List[String], tpe: String, frames: List[List[Int]], dmgsplit: List[List[Int]], atks: List[Int], atktpe: String, movetpe: Int, motiontpe: Int)
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
case class SkillTarget(tgt: Target, cls: TargetClass)
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

sealed trait StandardDamageEffect { self: HasActiveData =>
  def ratio: Int
  def scalingStat: UnitStats => Int
  def isMagic: Boolean
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
      (math.floor(math.pow(atk, 2) / (defs * (1 - itd))).toInt *
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
      val p = physical(atk,
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

  def calcHybrid(stats: BattleStats): Int = {
    hybrid(stats.unit.atk, stats.unit.mag, ratio, calcKillers(stats, _._1), calcElements(stats, true), 0, stats.target.defs, stats.target.spr, 0, 0, stats.unit.dw, stats.unit.level, stats.unit.l, stats.unit.r).hybrid
  }
  def calcPhysical(stats: BattleStats, canDW: Boolean): Int = {
    physical(stats.unit.atk, ratio / 100.0,
      calcKillers(stats, _._1), calcElements(stats, true),
      0, stats.target.defs, 0, canDW && stats.unit.dw,
      stats.unit.level, stats.unit.l, stats.unit.r)
  }
  def calcMagical(stats: BattleStats): Int = {
    (math.floor(math.pow(scalingStat(stats.unit), 2) / (stats.target.spr)).toInt *
      (1 + (calcKillers(stats, _._2) / 100.0)) * math.max(0, (1 + (calcElements(stats, false) / 100.0))) *
        (1 + (stats.unit.level / 100.0)) * ratio).toInt
  }
  def calculateDamage(stats: BattleStats): Int = {
    val data = self.data

    if (data.tpe == "ABILITY") {
      data.atktpe match {
        case "Physical" => if (isMagic)
          calcMagical(stats) * 2 else calcPhysical(stats, true)
        case "Hybrid"   => calcHybrid(stats)
        case "Magic"    => calcMagical(stats)
        case "None"     => // no DW, no elements, no killers
          // TODO calculate if fixed damage
          if (isMagic) calcMagical(stats) else calcPhysical(stats, false)
      }
    } else if (data.tpe == "MAGIC") {
      calcMagical(stats)
    } else {
      0
    }
  }
}

sealed trait Healing {
  def ratio: Int
  def base: Int
  def turns: Int
  def calculateHealing(stats: UnitStats): (Int,Int) = {
    val healing = (ratio.toDouble *
      (stats.spr * 0.5 + stats.mag * 0.1) + base).toInt / turns
    ((0.85 * healing).toInt, healing)
  }
}

case class HexDebuffAttackEffect(target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class InvokeSkillEffect(skill: Int) extends ActiveEffect with NoTarget
case class EntrustEffect(target: SkillTarget) extends ActiveEffect
case class EsperFillEffect(min: Int, max: Int, target: SkillTarget) extends ActiveEffect
case class HealChanceEffect(ratio: Int, base: Int, chance: Int, target: SkillTarget) extends ActiveEffect with Healing { def turns = 1 }
case class MPHealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect with Healing
case class SingingHealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class SingingMPHealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class HealEffect(ratio: Int, base: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class FixedDamageEffect(damage: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class HybridEffect(pratio: Int, mratio: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class DelayDamageEffect(ratio: Int, delay: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class JumpDamageEffect(ratio: Int, delay: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class MPDrainEffect(ratio: Int, drain: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class HPDrainEffect(ratio: Int, drain: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class SacrificeSelfRestoreEffect(hppct: Int, mppct: Int, target: SkillTarget) extends ActiveEffect
case class SacrificeHPPercentDamageEffect(sacrifice: Int, damage: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class SacrificeHPDamageEffect(ratio: Int, sacrifice: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
// skill -> chance
case class RandomActiveEffect(skills: List[(Int,Int)], target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class RandomMagicEffect(skills: List[(Int,Int)]) extends ActiveEffect with NoTarget
case class RestoreEffect(hp: Int, mp: Int, target: SkillTarget) extends ActiveEffect
case class SetHPEffect(hp: Int, target: SkillTarget) extends ActiveEffect
case class ReraiseEffect(pct: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class SalveEffect(items: List[Int], target: SkillTarget) extends ActiveEffect
case class RestorePercentEffect(hp: Int, mp: Int, target: SkillTarget) extends ActiveEffect
case class ReducePhysicalDamageEffect(pct: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class DodgePhysicalEffect(count: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class SkipTurnsEffect(turns: Int, target: SkillTarget) extends ActiveEffect
case object DualBlackMagicEffect extends ActiveEffect with NoTarget
case class LibraEffect(target: SkillTarget) extends ActiveEffect
case object DualCastEffect extends ActiveEffect with NoTarget
case class DualMagicEffect(white: Boolean, green: Boolean, whiteCount: Int, greenCount: Int) extends ActiveEffect with NoTarget
case class HideEffect(min: Int, max: Int, target: SkillTarget) extends ActiveEffect
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
case class PhysicalEffect(ratio: Int, itd: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class PhysicalKillerEffect(ratio: Int, tribe: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class MagicalKillerEffect(ratio: Int, tribe: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class StopEffect(chance: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class StealEffect(bonus: Int, target: SkillTarget) extends ActiveEffect
case class InstantKOEffect(chance: Int, target: SkillTarget) extends ActiveEffect
case class ElementResistEffect(fire: Int, ice: Int, lightning: Int, water: Int,
  wind: Int, earth: Int, light: Int, dark: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class EsunaEffect(poison: Boolean, blind: Boolean, sleep: Boolean, silence: Boolean, paralyze: Boolean, confusion: Boolean, disease: Boolean, petrify: Boolean, target: SkillTarget) extends ActiveEffect
case class StatusAilmentEffect(poison: Int, blind: Int, sleep: Int, silence: Int, paralyze: Int, confusion: Int, disease: Int, petrify: Int, target: SkillTarget) extends ActiveEffect
case class RandomAilmentEffect(poison: Int, blind: Int, sleep: Int, silence: Int, paralyze: Int, confusion: Int, disease: Int, petrify: Int, count: Int, target: SkillTarget) extends ActiveEffect
case class AilmentResistEffect(poison: Int, blind: Int, sleep: Int, silence: Int, paralyze: Int, confusion: Int, disease: Int, petrify: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class StackingPhysicalEffect(first: Int, stack: Int, max: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class StackingMagicalEffect(first: Int, stack: Int, max: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class MagicalEffect(ratio: Int, its: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class SingingBuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class BuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class RaiseEffect(pct: Int, target: SkillTarget) extends ActiveEffect
case class DebuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, target: SkillTarget) extends ActiveEffect
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
case class ActiveCounterEffect(chance: Int, stat: Int, turns: Int, max: Int, target: SkillTarget) extends ActiveEffect
case class PhysicalAllCoverEffect(chance: Int, preduction: Int, mreduction: Int, turns: Int, target: SkillTarget) extends ActiveEffect
case class UnlockSkillEffect(skill: Int, turns: Int) extends ActiveEffect with NoTarget
case class MultiAbilityEffect(skills: List[Int], count: Int) extends ActiveEffect with NoTarget
case class UnlockMultiSkillEffect(skills: List[Int], count: Int, turns: Int) extends ActiveEffect with NoTarget
case class ConditionalSkillEffect(trigger: List[Int], ifTrue: Int, ifFalse: Int) extends ActiveEffect with NoTarget
case class UnlockSkillCountedEffect(skills: List[Int], turns: Int, uses: Int) extends ActiveEffect with NoTarget
case class DamageOrDeathEffect(ratio: Int, chance: Int, death: Int, target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case class DeathImmunityEffect(turns: Int, target: SkillTarget) extends ActiveEffect
case class EvokeDamageEffect(magRatio: Int, sprRatio: Int, splits: List[Int], target: SkillTarget, data: ActiveData) extends ActiveEffect with HasActiveData
case object SurvivorFlaskEffect extends ActiveEffect with NoTarget
case object TwistOfFateEffect extends ActiveEffect with NoTarget

