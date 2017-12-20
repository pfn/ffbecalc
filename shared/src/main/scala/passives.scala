package yaffbedb

sealed trait SkillEffect {
  def restrictions: Set[Int]
  def canUse(unit: UnitData) = restrictions.isEmpty || restrictions(unit.id)
}
object SkillEffect {
  val SEX = Map("Male" -> 1, "Female" -> 2)
  val TRIBE = Map(
    1 ->  "Beast",
    2 ->  "Bird",
    3 ->  "Aquan",
    4 ->  "Demon",
    5 ->  "Man",
    6 ->  "Machine",
    7 ->  "Dragon",
    8 ->  "Spirit",
    9 ->  "Bug",
    10 -> "Stone",
    11 -> "Plant",
    12 -> "Undead"
  )

  val AILMENTS = Map(
    1 -> "Poison",
    2 -> "Blind",
    3 -> "Sleep",
    4 -> "Silence",
    5 -> "Paralyze",
    6 -> "Confusion",
    7 -> "Disease",
    8 -> "Petrify"
  )

  val ELEMENTS = Map(
    "Fire"      -> 1,
    "Ice"       -> 2,
    "Lightning" -> 3,
    "Water"     -> 4,
    "Wind"      -> 5,
    "Earth"     -> 6,
    "Light"     -> 7,
    "Dark"      -> 8,
  )

  val EQUIP = Map(
    1 -> "Dagger",
    2 -> "Sword",
    3 -> "Great Sword",
    4 -> "Katana",
    5 -> "Staff",
    6 -> "Rod",
    7 -> "Bow",
    8 -> "Axe",
    9 -> "Hammer",
    10 -> "Spear",
    11 -> "Harp",
    12 -> "Whip",
    13 -> "Throwing",
    14 -> "Gun",
    15 -> "Mace",
    16 -> "Fist",
    30 -> "L Shield",
    31 -> "H Shield",
    40 -> "Hat",
    41 -> "Helm",
    50 -> "Clothes",
    51 -> "L Armor",
    52 -> "H Armor",
    53 -> "Robe",
    60 -> "Accessory"
  )


  def apply(restrict: List[Int], x: Int, y: Int, z: Int, xs: List[Int]): SkillEffect = {
    (x, y, z) match {
      case (0 | 1, 3, 1)     => PassiveStatEffect.decode(restrict.toSet, xs)
      case (0 | 1, 3, 2)     => PassiveStatusResist.decode(restrict.toSet, xs)
      case (0 | 1, 3, 3)     => PassiveElementResist.decode(restrict.toSet, xs)
      case (0 | 1, 3, 5)     => PassiveEquipEffect.decode(xs)
      case (0 | 1, 3, 24)    => PassiveAttractEffect.decode(xs)
      case (0 | 1, 3, 10004) => PassiveWeapEleStatEffect.decode(xs)
      case (0 | 1, 3, 10003) => PassiveSinglehandEffect.decode(xs)
      case (0 | 1, 3, 6)     => PassiveEquipStatEffect.decode(xs)
      case (0 | 1, 1 | 3, 11) => PassiveKillerEffect.decode(xs)
      case (0 | 1, 3, 19)    => PassiveUnarmedEffect.decode(xs)
      case (0 | 1, 3, 21)    => PassiveEvoMagEffect.decode(xs)
      case (0 | 1, 3, 22)    => PassiveDodgeEffect.decode(xs)
      case (0 | 1, 3, 54)    => PassiveDodgeEffect.decode(xs)
      case (0 | 1, 3, 17)    => PassiveJumpEffect.decode(xs)
      case (0 | 1, 3, 14)    => PassiveDualWieldEffect.decode(xs)
      case (0 | 1, 3, 13)    => PassiveDoublehandEffect.decode(xs)
      case (0 | 1, 3, 25)    => PassiveCamouflageEffect.decode(restrict.toSet, xs)
      case (0 | 1, 3, 32)    => PassiveRefreshEffect.decode(restrict.toSet, xs)
      case (0 | 1, 3, 31)    => PassiveLimitBurstRateEffect.decode(xs)
      case (0 | 1, 3, 33)    => PassiveLimitBurstFillEffect.decode(xs)
      case _ => UnknownSkillEffect
    }
  }

  case class CollatedEffect(
                             elementResists: PassiveElementResist,
                             statusResists: PassiveStatusResist,
                             stats: PassiveStatEffect,
                             killers: Map[Int,(Int,Int)],
                             equipStats: Map[Int,PassiveStatEffect],
                             weapEleStats: Map[Int,PassiveStatEffect],
                             equips: Set[Int],
                             evomag: Int,
                             dodge: PassiveDodgeEffect,
                             jump: Int,
                             lbrate: Int,
                             lbfill: Int,
                             refresh: Int,
                             attract: Int,
                             camouflage: Int,
                             unarmed: PassiveStatEffect,
                             dh: PassiveDoublehandEffect,
                             dhGE: PassiveSinglehandEffect,
                             tdh: Passive2HEffect,
                             tdhGE: PassiveTDHEffect,
                             accuracy1h: Int,
                             dw: PassiveDualWieldEffect) {

    def isEmpty = this == CollatedEffect.empty
    def statFromEquips(eqs: List[EquipIndex]): PassiveStatEffect = {
      val ifUnarmed = if (eqs.forall(_.slotId != 1)) unarmed else PassiveStatEffect.zero

      eqs.foldLeft((PassiveStatEffect.zero,Set.empty[Int],Set.empty[Int])) { case ((ac,eqused,eleused), equip) =>
        val usedeq = eqused + equip.tpe
        val weapele = if (equip.slotId == 1)
          equip.stats.element.map { ELEMENTS.getOrElse(_, -1) }
        else Nil

        val usedele = eleused ++ weapele
        (ac + (if (!eqused(equip.tpe))
          equipStats.getOrElse(equip.tpe, PassiveStatEffect.zero)
        else PassiveStatEffect.zero) +
          weapele.foldLeft((PassiveStatEffect.zero,eleused)) {
            case ((ac2,used),e) =>
              val eff = if (used(e)) PassiveStatEffect.zero
              else weapEleStats.getOrElse(e, PassiveStatEffect.zero)

              (ac2 + eff,used + e)
          }._1, usedeq, usedele
        )
      }._1 + ifUnarmed
    }

    def canDualWield(weapon: Int) = dw.all || dw.weapons(weapon)
    def canEquip(tpe: Int, info: Option[UnitData]) =
      info.fold(false)(_.equipSet(tpe)) || equips(tpe)

    def dwString = {
      if (dw.all) "DualWield"
      else if (dw.weapons.nonEmpty) {
        val weaps = dw.weapons.toList.sorted.map(EQUIP.apply).mkString(", ")
        s"""DualWield($weaps)"""
      } else ""
    }

    def killerString = killers.toList.flatMap { case (k,(p,m)) =>
      val pk = if (p > 0) List(s"$p% ${TRIBE(k)}-Killer") else Nil
      val mk = if (m > 0) List(s"$m% Magic ${TRIBE(k)}-Killer") else Nil
      pk ++ mk
    }.mkString(" ")

    def tdhString = {
      if (tdh.dh != 0) List(s"""${tdh.dh}% Equip ATK w/ 1h or 2h""", s"+${tdh.accuracy}% accuracy") else Nil
    }.mkString(" ")

    def dhString = {
      if (dh.dh != 0) List(s"""${dh.dh}% Equip ATK w/ 1h""", s"+${dh.accuracy}% accuracy") else Nil
    }.mkString(" ")

    def dhGEString = {
      val items = dhGE.asList.groupBy(_._1).toList.map { case (k,v) =>
        s"""$k% Equip ${v.map(_._2).mkString("/")}"""
      }
      val dhs = if (items.nonEmpty) items :+ "w/ 1h"
      else items
      val acc = if (accuracy1h > 0) List(s"+$accuracy1h% accuracy") else Nil
      dhs ++ acc
    }.mkString(" ")

    def tdhGEString = {
      val items = tdhGE.asList.groupBy(_._1).toList.map { case (k,v) =>
        s"""$k% Equip ${v.map(_._2).mkString("/")}"""
      }
      val dhs = if (items.nonEmpty) items :+ "w/ 1h or 2h"
      else items
      val acc = if (accuracy1h > 0) List(s"+$accuracy1h% accuracy") else Nil
      dhs ++ acc
    }.mkString(" ")

    def refreshString = if (refresh > 0) s"$refresh% Auto-Refresh" else ""
    def camoString = if (camouflage > 0) s"$camouflage% Camouflage" else ""
    def dodgeString = {
      val m = if (dodge.mag > 0) List(s"${dodge.mag}% Magic Evasion") else Nil
      val p = if (dodge.phys > 0) List(s"${dodge.phys}% Dodge") else Nil
      (p ++ m).mkString(" ")
    }

    def lbrateString = if (lbrate > 0) s"$lbrate% LB fill" else ""
    def lbfillString = if (lbfill > 0) s"${lbfill/100} LB/turn" else ""
    def attractString = if (attract > 0) s"$attract% Attract" else ""
    def equipStatsString =
      equipStats.keys.map(k => s"${equipStats(k)} w/ ${EQUIP(k)}").mkString(", ")

    override lazy val toString = List(
      dwString,
      dhString,
      tdhString,
      dhGEString,
      tdhGEString,
      stats.toString,
      equipStatsString,
      dodgeString,
      killerString,
      elementResists.asElementResist.toString,
      statusResists.asAilmentResist.toString,
      lbrateString,
      lbfillString,
      refreshString,
      camoString,
      attractString
    ).filter(_.trim.nonEmpty).mkString(", ")
  }
  object CollatedEffect {
    def empty: CollatedEffect = CollatedEffect(
      PassiveElementResist(Set.empty, 0, 0, 0, 0, 0, 0, 0, 0),
      PassiveStatusResist(Set.empty, 0, 0, 0, 0, 0, 0, 0, 0),
      PassiveStatEffect.zero,
      Map.empty,
      Map.empty,
      Map.empty,
      Set.empty,
      0,
      PassiveDodgeEffect(0, 0),
      0,
      0,
      0,
      0,
      0,
      0,
      PassiveStatEffect.zero,
      PassiveDoublehandEffect.zero,
      PassiveSinglehandEffect.zero,
      Passive2HEffect.zero,
      PassiveTDHEffect.zero,
      0,
      PassiveDualWieldEffect(Set.empty, false))
  }

  def collateEffects(unit: Option[UnitData], xs: List[SkillEffect]) = {
    xs.filter(e => unit.forall(e.canUse)).foldLeft(CollatedEffect.empty) { (a, eff) => eff match {
      case e@PassiveElementResist(_,_, _, _, _, _, _, _, _) =>
        a.copy(elementResists = a.elementResists + e)
      case e@PassiveStatusResist(_,_,_,_,_,_,_,_,_) =>
        a.copy(statusResists = a.statusResists + e)
      case e@PassiveStatEffect(_,_,_,_,_,_,_,_) =>
        a.copy(stats = a.stats + e)
      case e@PassiveWeapEleStatEffect(_,_,_,_,_,_,_) =>
        val eff = a.weapEleStats.getOrElse(e.element, PassiveStatEffect.zero)
        a.copy(weapEleStats = a.weapEleStats + ((e.element, eff + e.asPassiveStatEffect)))
      case e@PassiveEquipStatEffect(_,_,_,_,_,_,_) =>
        val eff = a.equipStats.getOrElse(e.cond, PassiveStatEffect.zero)
        a.copy(equipStats = a.equipStats +
          ((e.cond, eff + e.asPassiveStatEffect)))
      case PassiveKillerEffect(tribe, phys, mag) =>
        val (p,m) = a.killers.getOrElse(tribe, (0,0))
        a.copy(killers = a.killers + ((tribe, (p + phys, m + mag))))
      case PassiveEvoMagEffect(evomag) =>
        a.copy(evomag = a.evomag + evomag)
      case PassiveEquipEffect(equip) =>
        a.copy(equips = a.equips + equip)
      case PassiveDodgeEffect(p, m) =>
        a.copy(dodge = PassiveDodgeEffect(p + a.dodge.phys, m + a.dodge.mag))
      case PassiveJumpEffect(j) =>
        a.copy(jump = a.jump + j)
      case PassiveAttractEffect(d) =>
        a.copy(attract = a.attract + d)
      case PassiveLimitBurstFillEffect(mod) => a.copy(lbfill = a.lbfill + mod)
      case PassiveLimitBurstRateEffect(mod) => a.copy(lbrate = a.lbrate + mod)
      case PassiveCamouflageEffect(_, mod) => a.copy(camouflage = a.camouflage + mod)
      case PassiveRefreshEffect(_, mod) => a.copy(refresh = a.refresh + mod)
      case sh@PassiveSinglehandEffect(_,_,_,_,_,_) => a.copy(dhGE = a.dhGE + sh)
      case sh@PassiveTDHEffect(_,_,_,_,_,_) => a.copy(tdhGE = a.tdhGE + sh)
      case Passive2HEffect(dh, acc) =>
        a.copy(tdh = a.tdh.copy(dh = a.tdh.dh + dh, accuracy = a.tdh.accuracy + acc))
      case dh@PassiveDoublehandEffect(_, _) =>
        a.copy(dh = a.dh + dh)
      case PassiveDualWieldEffect(weaps, all) =>
        a.copy(dw = PassiveDualWieldEffect(a.dw.weapons ++ weaps, all || a.dw.all))
      case PassiveUnarmedEffect(sts) =>
        a.copy(unarmed = a.unarmed + sts)
      case _ => a
    }}
  }
}
case object UnknownSkillEffect extends SkillEffect {
  override def restrictions = Set.empty
}

sealed trait NoRestrictions {
  def restrictions = Set.empty[Int]
}
object PassiveElementResist {
  def decode(restrict: Set[Int], xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c, d, e, f, g, h) =>
      PassiveElementResist(restrict, a, b, c, d, e, f, g, h)
    case List(_, _, _, _, _, _) =>
      // this seems to be a mistake from skill 501890
      // counter skill that's listed as !active but has active effects_raw
      //PassiveElementResist(restrict, a, b, c, d, e, f, 0, 0)
      UnknownSkillEffect
  }
}
object PassiveStatusResist {
  def decode(restrict: Set[Int], xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c, d, e, f, g, h, _) =>
      PassiveStatusResist(restrict, a, b, c, d, e, f, g, h)
    case List(a, b, c, d, e, f, g, h) =>
      PassiveStatusResist(restrict, a, b, c, d, e, f, g, h)
    case List(a) =>
      PassiveStatusResist(restrict, a, 0, 0, 0, 0, 0, 0, 0)
  }
}
case class PassiveElementResist(
                                 restrictions: Set[Int],
                                 fire: Int,
                                 ice: Int,
                                 lightning: Int,
                                 water: Int,
                                 wind: Int,
                                 earth: Int,
                                 light: Int,
                                 dark: Int
                               ) extends SkillEffect {
  def +(o: PassiveElementResist) =
    PassiveElementResist(restrictions ++ o.restrictions, fire + o.fire, ice + o.ice, lightning + o.lightning,
      water + o.water, wind + o.wind, earth + o.earth,
      light + o.light, dark + o.dark)
  def asElementResist = ElementResist(fire, ice, lightning, water, wind, earth, light, dark)
}
case class PassiveStatusResist(
                                restrictions: Set[Int],
                                poison: Int,
                                blind: Int,
                                sleep: Int,
                                silence: Int,
                                paralyze: Int,
                                confusion: Int,
                                disease: Int,
                                petrify: Int
                              ) extends SkillEffect {
  def +(o: PassiveStatusResist) = PassiveStatusResist(
    restrictions ++ o.restrictions,
    poison + o.poison,
    blind + o.blind,
    sleep + o.sleep,
    silence + o.silence,
    paralyze + o.paralyze,
    confusion + o.confusion,
    disease + o.disease,
    petrify + o.petrify)
  def asAilmentResist = AilmentResist(poison, blind, sleep, silence, paralyze, confusion, disease, petrify)
}
object PassiveWeapEleStatEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c, d, e, f, g) =>
      PassiveWeapEleStatEffect(a, b, c, d, e, f, g)
  }
}
case class PassiveWeapEleStatEffect(
                                     element: Int,
                                     hp:   Int,
                                     mp:   Int,
                                     atk:  Int,
                                     mag:  Int,
                                     defs: Int,
                                     spr:  Int) extends SkillEffect with NoRestrictions {
  def asPassiveStatEffect =
    PassiveStatEffect(Set.empty, hp, mp, atk, defs, mag, spr, 0)
}
object PassiveEquipEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveEquipEffect(a)
  }
}
case class PassiveEquipEffect(equip: Int) extends SkillEffect with NoRestrictions
object PassiveEquipStatEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c, d, e) =>
      PassiveEquipStatEffect(a, 0, 0, b, c, d, e)
    case List(a, atk, defs, mag, spr, hp) =>
      PassiveEquipStatEffect(a, hp, 0, atk, defs, mag, spr)
    case List(a, atk, defs, mag, spr, hp, mp) =>
      PassiveEquipStatEffect(a, hp, mp, atk, defs, mag, spr)
    case List(a, atk, defs, mag, spr, hp, mp, _) => // 8 args, last is???
      PassiveEquipStatEffect(a, hp, mp, atk, defs, mag, spr)
    case List(a, atk, defs, mag, spr, hp, mp, _, _) => // 9 args, last 2 are???
      PassiveEquipStatEffect(a, hp, mp, atk, defs, mag, spr)
    case List(a, b, c, d, e, f, g, _) =>
      PassiveEquipStatEffect(a, f, g, b, c, d, e)
  }
}
case class PassiveSinglehandEffect(hp: Int, mp: Int, atk: Int, defs: Int, mag: Int, spr: Int) extends SkillEffect with NoRestrictions {
  def +(o: PassiveSinglehandEffect) = PassiveSinglehandEffect(hp + o.hp, mp + o.mp, atk + o.atk, defs + o.defs, mag + o.mag, spr + o.spr)
  def asList = List(
    hp   -> "HP",
    mp   -> "MP",
    atk  -> "ATK",
    defs -> "DEF",
    mag  -> "MAG",
    spr  -> "SPR",
  ).filterNot(_._1 == 0)
}
object PassiveSinglehandEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c, d, e, f) =>
      PassiveSinglehandEffect(a, b, c, e, d, f)
    case List(a, b, c, d, e, f, is2H) =>
      if (is2H == 1)
        PassiveTDHEffect(a, b, c, e, d, f)
      else
        PassiveSinglehandEffect(a, b, c, e, d, f)
  }
  def zero = PassiveSinglehandEffect(0, 0, 0, 0, 0, 0)
}
case class PassiveTDHEffect(hp: Int, mp: Int, atk: Int, defs: Int, mag: Int, spr: Int) extends SkillEffect with NoRestrictions {
  def +(o: PassiveTDHEffect) = PassiveTDHEffect(hp + o.hp, mp + o.mp, atk + o.atk, defs + o.defs, mag + o.mag, spr + o.spr)
  def asList = List(
    hp   -> "HP",
    mp   -> "MP",
    atk  -> "ATK",
    defs -> "DEF",
    mag  -> "MAG",
    spr  -> "SPR",
  ).filterNot(_._1 == 0)

  def asSingleHand = PassiveSinglehandEffect(hp, mp, atk, defs, mag, spr)
}
object PassiveTDHEffect {
  def zero = PassiveTDHEffect(0, 0, 0, 0, 0, 0)
}

case class PassiveDoublehandEffect(dh: Int, accuracy: Int) extends SkillEffect with NoRestrictions {
  def +(o: PassiveDoublehandEffect) =
    PassiveDoublehandEffect(dh + o.dh, accuracy + o.accuracy)
  def asSingleHand = PassiveSinglehandEffect(0, 0, dh, 0, 0, 0)
}
object PassiveDoublehandEffect {
  def zero = PassiveDoublehandEffect(0, 0)
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveDoublehandEffect(a, 0)
    // TODO handle b and c: accuracy, c == 2 => 2h
    case List(a, b, c) =>
      if (c == 2) Passive2HEffect(a, b)
      else PassiveDoublehandEffect(a, b)
  }
}
case class Passive2HEffect(dh: Int, accuracy: Int) extends SkillEffect with NoRestrictions {
  def asSingleHand = PassiveSinglehandEffect(0, 0, dh, 0, 0, 0)
}
object Passive2HEffect {
  def zero = Passive2HEffect(0, 0)
}
case class PassiveEquipStatEffect(
                                   cond: Int,
                                   hp:   Int,
                                   mp:   Int,
                                   atk:  Int,
                                   defs: Int,
                                   mag:  Int,
                                   spr:  Int) extends SkillEffect {
  def restrictions = Set.empty
  def asPassiveStatEffect =
    PassiveStatEffect(Set.empty, hp, mp, atk, defs, mag, spr, 0)
}
object PassiveStatEffect {
  def decode(restrict: Set[Int], xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c, d, e, f, g) =>
      PassiveStatEffect(restrict, e, f, a, b, c, d, g)
    case List(a, b, c, d, e, f) =>
      PassiveStatEffect(restrict, e, f, a, b, c, d, 0)
  }

  def zero = PassiveStatEffect(Set.empty, 0, 0, 0, 0, 0, 0, 0)
}
case class PassiveStatEffect(
                              restrictions: Set[Int],
                              hp:   Int,
                              mp:   Int,
                              atk:  Int,
                              defs: Int,
                              mag:  Int,
                              spr:  Int,
                              crit: Int) extends SkillEffect {
  def +(o: PassiveStatEffect) = PassiveStatEffect(restrictions ++ o.restrictions, hp + o.hp, mp + o.mp,
    atk + o.atk, defs + o.defs, mag + o.mag, spr + o.spr, crit + o.crit)
  def asList = List(
    hp   -> "HP",
    mp   -> "MP",
    atk  -> "ATK",
    defs -> "DEF",
    mag  -> "MAG",
    spr  -> "SPR",
    crit -> "Crit"
  ).filterNot(_._1 == 0)

  override lazy val toString = asList.groupBy(_._1).toList.map { case (k,v) =>
    s"""$k% ${v.map(_._2).mkString("/")}"""
  }.mkString(" ")
}

case class PassiveUnarmedEffect(stats: PassiveStatEffect) extends SkillEffect with NoRestrictions
object PassiveUnarmedEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c, d) =>
      PassiveUnarmedEffect(PassiveStatEffect(Set.empty, 0, 0, a, b, c, d, 0))
    case List(a) =>
      PassiveUnarmedEffect(PassiveStatEffect(Set.empty, 0, 0, a, 0, 0, 0, 0))
  }
}

object PassiveKillerEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a, b, c) =>
      PassiveKillerEffect(a, b, c)
  }
}
case class PassiveKillerEffect(tribe: Int, phys: Int, mag: Int)
  extends SkillEffect with NoRestrictions
object PassiveEvoMagEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) =>
      PassiveEvoMagEffect(a)
  }
}
case class PassiveEvoMagEffect(evomag: Int) extends SkillEffect with NoRestrictions
object PassiveDodgeEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveDodgeEffect(a, 0)
    case List(_, b) => PassiveDodgeEffect(0, b)
  }
}
case class PassiveDodgeEffect(phys: Int, mag: Int) extends SkillEffect with NoRestrictions
object PassiveCamouflageEffect {
  def decode(restrict: Set[Int], xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveCamouflageEffect(restrict, a)
  }
}
case class PassiveCamouflageEffect(restrictions: Set[Int], camo: Int) extends SkillEffect
object PassiveRefreshEffect {
  def decode(restrict: Set[Int], xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveRefreshEffect(restrict, a)
  }
}
case class PassiveRefreshEffect(restrictions: Set[Int], refresh: Int) extends SkillEffect
object PassiveJumpEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveJumpEffect(a)
  }
}
case class PassiveJumpEffect(mod: Int) extends SkillEffect with NoRestrictions
object PassiveAttractEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveAttractEffect(a)
  }
}
case class PassiveAttractEffect(mod: Int) extends SkillEffect with NoRestrictions
object PassiveLimitBurstRateEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveLimitBurstRateEffect(a)
  }
}
object PassiveLimitBurstFillEffect {
  def decode(xs: List[Int]): SkillEffect = xs match {
    case List(a) => PassiveLimitBurstFillEffect(a)
  }
}
case class PassiveLimitBurstFillEffect(mod: Int) extends SkillEffect with NoRestrictions
case class PassiveLimitBurstRateEffect(mod: Int) extends SkillEffect with NoRestrictions
object PassiveDualWieldEffect {
  def decode(xs: List[Int]): SkillEffect =
    if (xs.isEmpty) PassiveDualWieldEffect(Set.empty, true)
    else PassiveDualWieldEffect(xs.toSet, false)
}
case class PassiveDualWieldEffect(weapons: Set[Int], all: Boolean) extends SkillEffect with NoRestrictions
