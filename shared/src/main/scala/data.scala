package com.ffbecalc

case class UnitIndex(name: String, min: Int, max: Int, id: String)
case class UnitIndexData(min: Int, max: Int, id: String)
case class UnitSkill(rarity: Int, level: Int, tpe: String, id: Int)
case class UnitStrings(
  description: List[Option[String]],
  summon: List[Option[String]],
  evolution: List[Option[String]],
  affinity: List[Option[String]],
  fusion: List[Option[String]])
sealed trait TMR { def id: Int }
sealed trait Equipment {
  def id: Int
  def name: String
  def icon: String
}
case class LimitBurstEffect(cost: Int, effects: List[String], actives: List[ActiveEffect])
case class LimitBurst(name: String, levels: Int, min: LimitBurstEffect, max: LimitBurstEffect)
case class MateriaTrust(id: Int) extends TMR
case class EquipTrust(id: Int) extends TMR
case class UnitEntry(
  rarity: Int,
  stats: StatInfo,
  lb: Int,
  abilitySlots: Int,
  magicAffinity: MagicAffinity,
  elementResist: ElementResist,
  statusResist: AilmentResist,
  strings: UnitStrings) {
  def canEquip(m: MateriaIndex): Boolean = m.magicType.fold(true) {
    case "White" => magicAffinity.white >= m.rarity
    case "Black" => magicAffinity.black >= m.rarity
    case "Green" => magicAffinity.green >= m.rarity
    case "Blue"  => magicAffinity.blue  >= m.rarity
    case _       => false
  }
}
case class UnitData(
  name: String,
  id: Int,
  job: String,
  sex: String,
  tmr: Option[TMR],
  equip: List[Int],
  entries: Map[String,UnitEntry],
  skills: List[UnitSkill]) {
  lazy val equipSet = equip.toSet
  override def equals(o: Any) = o match {
    case d: UnitData => d.id == id
    case _ => false
  }

  override def hashCode() = id
}
case class EsperData(names: List[String], entries: List[EsperEntry])
case class EsperEntry(
  stats: EsperStatInfo,
  elementResist: ElementResist,
  statusResist: AilmentResist
)
sealed trait EsperSkill
object EsperStatReward {
  def hp(x: Int)   = EsperStatReward(x, 0, 0, 0, 0, 0)
  def mp(x: Int)   = EsperStatReward(0, x, 0, 0, 0, 0)
  def atk(x: Int)  = EsperStatReward(0, 0, x, 0, 0, 0)
  def defs(x: Int) = EsperStatReward(0, 0, 0, x, 0, 0)
  def mag(x: Int)  = EsperStatReward(0, 0, 0, 0, x, 0)
  def spr(x: Int)  = EsperStatReward(0, 0, 0, 0, 0, x)
}
case class EsperAbilityReward(skill: Int) extends EsperSkill
case class EsperSkillEffectReward(skillInfo: SkillInfo) extends EsperSkill
case class EsperStatReward(hp: Int, mp: Int, atk: Int, defs: Int, mag: Int, spr: Int) extends EsperSkill {
  def maybeHP  = if (hp   > 0) Some(hp)   else None
  def maybeMP  = if (mp   > 0) Some(mp)   else None
  def maybeATK = if (atk  > 0) Some(atk)  else None
  def maybeDEF = if (defs > 0) Some(defs) else None
  def maybeMAG = if (mag  > 0) Some(mag)  else None
  def maybeSPR = if (spr  > 0) Some(spr)  else None
}
case object UnknownEsperSkill extends EsperSkill
case class EsperSlot(
  reward: EsperSkill,
  cost:   Int
)

case class EsperStatInfo(
  hp:   EsperStatRange,
  mp:   EsperStatRange,
  atk:  EsperStatRange,
  defs: EsperStatRange,
  mag:  EsperStatRange,
  spr:  EsperStatRange) {
  def +(o: EsperStatReward) = EsperStatInfo(hp + o.hp, mp + o.mp, atk + o.atk, defs + o.defs, mag + o.mag, spr + o.spr)
}
case class EsperStatRange(min: Int, max: Int) {
  def effectiveMax = max / 100
  def +(o: Int) = EsperStatRange(min + o, max + o)
}
case class IndexSkillInfo(
  id: Int,
  name: String,
  unique: Boolean,
  icon: String,
  effects: List[String],
  actives: List[ActiveEffect],
  passives: List[SkillEffect]
)
case class SkillInfo(
  id: Int,
  name: String,
  unique: Boolean,
  active: Boolean,
  tpe: String,
  icon: String,
  magicType: Option[String],
  mpCost: Int,
  actives: List[ActiveEffect],
  passives: List[SkillEffect],
  effects: List[String]) {
  def asIndexSkillInfo =
    IndexSkillInfo(id, name, unique, icon, effects, actives, passives)
}
case class EnhancementStrings(name: List[String], desc: List[String])
case class Enhancement(oldSkill: Int, newSkill: Int, strings: EnhancementStrings)
sealed trait SkillIndex { def skillInfo: List[IndexSkillInfo] }
case class MateriaIndexData(id: String, icon: String, unique: Boolean, rarity: Int, magicType: Option[String], skillInfo: List[IndexSkillInfo])
case class MateriaIndex(name: String, id: Int, icon: String, unique: Boolean, rarity: Int, magicType: Option[String], skillInfo: List[IndexSkillInfo]) extends Equipment with SkillIndex {
  val memo = Memo { a: Option[UnitData] =>
    SkillEffect.collateEffects(a, skillInfo.flatMap(_.passives)).toString
  }

  def describeEffects(unit: Option[UnitData]) = memo(unit)
}
case class EquipStats(
  hp: Int, mp: Int, atk: Int, defs: Int, mag: Int, spr: Int,
  elementResists: Option[EquipElementResist],
  statusResists: Option[EquipAilments],
  statusEffects: Option[EquipAilments],
  element: List[String]) {
  override lazy val toString = {
    val ss = ((if (atk != 0) List(s"ATK+$atk") else Nil) ++
    (if (defs != 0) List(s"DEF+$defs") else Nil) ++
    (if (mag != 0) List(s"MAG+$mag") else Nil) ++
    (if (spr != 0) List(s"SPR+$spr") else Nil) ++
    (if (hp != 0) List(s"HP+$hp") else Nil) ++
    (if (mp != 0) List(s"MP+$mp") else Nil)).mkString(" ")
    val status = statusResists.fold("")(_.asAilmentResist.toString)
    val eleres = elementResists.fold("")(_.asElementResist.toString)
    val ele = if (element.isEmpty) "" else "+" + element.mkString("/")
    val effects = statusEffects.fold("")(_.toString)
    List(ss, ele, effects, status, eleres).filter(_.trim.nonEmpty).mkString(", ")
  }

  def elementResist = elementResists.fold(ElementResist.zero)(_.asElementResist)
  def ailmentResist = statusResists.fold(AilmentResist.zero)(_.asAilmentResist)
}
sealed trait EquipReq {
  def canEquip(unit: UnitData): Boolean
}
case class SexEquipReq(sex: Int) extends EquipReq {
  def canEquip(unit: UnitData) = SkillEffect.SEX.getOrElse(unit.sex, -1) == sex
}
case class UnitEquipReq(id: Int) extends EquipReq {
  def canEquip(unit: UnitData) = unit.id == id
}
case class Memo[A,B](f: A => B) extends Function1[A,B] {
  var memo = Map.empty[A,B]
  def apply(a: A): B = memo.getOrElse(a, {
    val m = f(a)
    memo += a -> m
    m
  })
}
case class WeaponVariance(min: Double, max: Double) {
  def effective(tpe: Int) =
    if (min == 0 && max == 0) SkillEffect.VARIANCE(tpe) else this
}
object WeaponVariance {
  def none = WeaponVariance(0, 0)
}
case class EquipIndexData(
  id: Int, icon: String, slotId: Int, twohands: Boolean, variance: WeaponVariance, accuracy: Int, skills: List[Int], tpe: Int, stats: EquipStats, req: Option[EquipReq], skillInfo: List[IndexSkillInfo])
case class EquipIndex(
  name: String, id: Int, icon: String, twohands: Boolean, slotId: Int, variance: WeaponVariance, accuracy: Int, skills: List[Int], tpe: Int, stats: EquipStats, req: Option[EquipReq], skillInfo: List[IndexSkillInfo]) extends Equipment with SkillIndex {

  val memo = Memo { a: Option[UnitData] =>
    SkillEffect.collateEffects(a, skillInfo.flatMap(_.passives)).toString
  }

  def describeEffects(unit: Option[UnitData]) = memo(unit)

  def canEquip(unit: Option[UnitData]) =
    unit.fold(true)(u => req.forall(_.canEquip(u)))
}
case class MagicAffinity(white: Int, black: Int, green: Int, blue: Int)
case class StatRange(min: Int, max: Int, pots: Int) {
  def maxpots = max + pots
}

case class StatInfo(hp: StatRange,
  mp: StatRange,
  atk: StatRange,
  mag: StatRange,
  defs: StatRange,
  spr: StatRange)
case class EquipAilments(
  poison: Option[Int],
  blind: Option[Int],
  sleep: Option[Int],
  silence: Option[Int],
  paralyze: Option[Int],
  confusion: Option[Int],
  disease: Option[Int],
  petrify: Option[Int]) {
  def asAilmentResist = AilmentResist(poison.getOrElse(0),
    blind.getOrElse(0),
    sleep.getOrElse(0),
    silence.getOrElse(0),
    paralyze.getOrElse(0),
    confusion.getOrElse(0),
    disease.getOrElse(0),
    petrify.getOrElse(0))
  def asList = List(
    poison    -> "Poison",
    blind     -> "Blind",
    sleep     -> "Sleep",
    silence   -> "Silence",
    paralyze  -> "Paralyze",
    confusion -> "Confusion",
    disease   -> "Disease",
    petrify   -> "Petrify")
  override lazy val toString = {
    val items = asList.filterNot(_._1.getOrElse(0) == 0).groupBy(_._1.getOrElse(0)).toList.map {
      case (k,v) =>
        val res = if (v.size == 8) "All Ailments"
        else v.map(_._2).mkString("/")
        s"""$k% $res"""
    }
    items.mkString(", ")
  }
}

case class AilmentResist(
  poison: Int,
  blind: Int,
  sleep: Int,
  silence: Int,
  paralyze: Int,
  confusion: Int,
  disease: Int,
  petrify: Int) {
  def +(o: AilmentResist) = AilmentResist(poison + o.poison, blind + o.blind, sleep + o.sleep, silence + o.silence, paralyze + o.paralyze, confusion + o.confusion, disease + o.disease, petrify + o.petrify)
  def asList = List(
    poison    -> "Poison",
    blind     -> "Blind",
    sleep     -> "Sleep",
    silence   -> "Silence",
    paralyze  -> "Paralyze",
    confusion -> "Confusion",
    disease   -> "Disease",
    petrify   -> "Petrify")
  override lazy val toString = {
    val items = asList.filterNot(_._1 == 0).groupBy(_._1).toList.map {
      case (k,v) =>
        val res = if (v.size == 8) "All Ailments"
        else v.map(_._2).mkString("/")
        s"""$k% $res Resist"""
    }
    items.mkString(", ")
  }
}
object AilmentResist {
  def zero = AilmentResist(0, 0, 0, 0, 0, 0, 0, 0)
}
case class EquipElementResist(
  fire: Option[Int],
  ice: Option[Int],
  lightning: Option[Int],
  water: Option[Int],
  wind: Option[Int],
  earth: Option[Int],
  light: Option[Int],
  dark: Option[Int]) {
  def asElementResist = ElementResist(fire.getOrElse(0),
    ice.getOrElse(0),
    lightning.getOrElse(0),
    water.getOrElse(0),
    wind.getOrElse(0),
    earth.getOrElse(0),
    light.getOrElse(0),
    dark.getOrElse(0))
}
case class ElementResist(
  fire: Int,
  ice: Int,
  lightning: Int,
  water: Int,
  wind: Int,
  earth: Int,
  light: Int,
  dark: Int) {
  def +(o: ElementResist) = ElementResist(fire + o.fire, ice + o.ice, lightning + o.lightning, water + o.water, wind + o.wind, earth + o.earth, light + o.light, dark + o.dark)
  def asMap = Map(
    1 -> fire,
    2 -> ice,
    3 -> lightning,
    4 -> water,
    5 -> wind,
    6 -> earth,
    7 -> light,
    8 -> dark
  )
  def asList = List(
    fire      -> "Fire",
    ice       -> "Ice",
    lightning -> "Lightning",
    water     -> "Water",
    wind      -> "Wind",
    earth     -> "Earth",
    light     -> "Light",
    dark      -> "Dark"
  )
  override lazy val toString = {
    val items = asList.filterNot(_._1 == 0).groupBy(_._1).toList.map {
      case (k,v) =>
        val res = if (v.size == 8) "All Elements"
        else v.map(_._2).mkString("/")
        s"""$k% $res Resist"""
    }
    items.mkString(", ")
  }
}
object ElementResist {
  def zero = ElementResist(0, 0, 0, 0, 0, 0, 0, 0)
}
