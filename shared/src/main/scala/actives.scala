package yaffbedb

sealed trait Target
object Target {
  case object Self extends Target
  case object Team extends Target
  case object Single extends Target
  case object AoE extends Target
  case object Random extends Target
}
sealed trait Condition
sealed trait ActiveEffect
sealed trait AttackEffect {
  def elements: List[String]
}
case object UnknownActiveEffect extends ActiveEffect
trait ConditionalEffect
trait ConditionalBuff
trait DamageEffect
sealed trait HealScaling
case object StandardHealing extends HealScaling
case class HealEffect(ratio: Int, base: Int, turns: Int, aoe: Boolean, scaling: HealScaling = StandardHealing) extends ActiveEffect
object HealEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(_, _, c, d, _) => HealEffect(d, c, 1, aoe)
    case List(a, _, c, d)    => HealEffect(a, c, d, aoe)
  }
}
case class HybridEffect(pratio: Int, mratio: Int, elements: List[String], aoe: Boolean) extends ActiveEffect with AttackEffect
object HybridEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(a, b, c, d, e, f, g, h, i, j) => HybridEffect(i, j, Nil, aoe)
  }

}
case class PhysicalEffect(ratio: Int, itd: Int, elements: List[String], aoe: Boolean) extends ActiveEffect with AttackEffect
object PhysicalEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(a, b, c, d) => PhysicalEffect(c, d, Nil, aoe)
    case List(_, _, _, _, _, _, ratio) => PhysicalEffect(ratio, 0, Nil, aoe)
    case List(_, _, _, _, _, _, ratio, _) => PhysicalEffect(ratio, 0, Nil, aoe)
  }
}
case class StopEffect(chance: Int, turns: Int, aoe: Boolean) extends ActiveEffect
object StopEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(chance, turns) => StopEffect(chance, turns, aoe)
  }
}
case class ImperilEffect(fire: Int, ice: Int, lightning: Int, water: Int,
                         wind: Int, earth: Int, light: Int, dark: Int, turns: Int, aoe: Boolean) extends ActiveEffect
object ImperilEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(fire, ice, lightning, water, wind, earth, light, dark, _, turns) =>
      ImperilEffect(fire, ice, lightning, water, wind, earth, light, dark, turns, aoe)
  }
}
case class MagicalEffect(ratio: Int, its: Int, elements: List[String], aoe: Boolean) extends ActiveEffect with AttackEffect
object MagicalEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(_, _, _, _, _, f) => MagicalEffect(f, 0, Nil, aoe)
    case List(_, _, _, _, _, f, _) => MagicalEffect(f, 0, Nil, aoe)
    case List(_, _, ratio, its) => MagicalEffect(ratio, its, Nil, aoe)
  }
}
case class BuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, aoe: Boolean) extends ActiveEffect
object BuffEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(a, b, c, d, e, _) => BuffEffect(a, b, c, d, e, aoe)
  }
}
case class DebuffEffect(atk: Int, defs: Int, mag: Int, spr: Int, turns: Int, aoe: Boolean) extends ActiveEffect
object DebuffEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(a, b, c, d, e, _) => DebuffEffect(a, b, c, d, e, aoe)
  }
}


object ActiveEffects {
  // 1st arg, targeting flag, 0 = self, 1 = ST, 2 = AOE, 3 = random
  // 2nd arg, target type, 1 = enemy, 2 = allies, 3 = self, 5 = allies no-self, 6 = ko'd, enemy-reaper

  // repeated count = range min-max
  def apply(x: Int, y: Int, z: Int, xs: List[Int]): ActiveEffect = (x, y, z) match {
    case (aoeflag, 1, 1) => PhysicalEffect.decode(aoeflag == 2, xs)
    case (aoeflag, 2, 2) => HealEffect.decode(aoeflag == 2, xs)
    case (aoeflag, 6, 2) => HealEffect.decode(aoeflag == 2, xs)
    case (_, 2, 3) => UnknownActiveEffect // BuffEffect.decode(tgt, xs)
    case (_, 3, 3) => UnknownActiveEffect // BuffEffect.decode(tgt, xs)
    case (_, 6, 4) => UnknownActiveEffect // raise effect, args pct heal
    case (_, 2, 5) => UnknownActiveEffect // ally status ailments cure, args 1 2 3 4 5 6 7 8
    case (0, 3, 5) => UnknownActiveEffect // self ailments cure, args 1 2 3 4 5 6 7 8 (or 0 for not cure)
    case (_, 1, 6) => UnknownActiveEffect // StatusAilmentEffect
    case (aoeflag, 2, 8) => HealEffect.decode(aoeflag == 2, xs)
    case (_, 1, 9) => UnknownActiveEffect // pcthp damage, pct pct 100?
    case (_, 1, 10) => UnknownActiveEffect // physical osmose drainratio ratio 100?
    case (_, 1, 13) => UnknownActiveEffect // delay damage, args: delay 0? 0? 2? ??? ratio
    case (aoeflag, 1, 15) => MagicalEffect.decode(aoeflag == 2, xs)
    case (_, 3, 17) => UnknownActiveEffect // mp heal, self? args N
    case (0, 3, 20) => UnknownActiveEffect // store +ratio maxratio 0?
    case (aoeflag, 1, 21) => PhysicalEffect.decode(aoeflag == 2, xs)
    case (_, 1, 23) => UnknownActiveEffect // magic damage, bonus to type, args: type 0? 0? bonus-ratio
    case (aoeflag, 1, 24) => DebuffEffect.decode(aoeflag == 2, xs)
    case (_, 1, 25) => UnknownActiveEffect // physical drain drainratio ratio 100?
    case (_, 2, 27) => UnknownActiveEffect // reraise, args pct, turns
    case (_, 2, 28) => UnknownActiveEffect // salve, args = valid item ids
    case (_, 2, 30) => UnknownActiveEffect // MP regen, args: scale, 1?, base, turns
    case (_, 3, 30) => UnknownActiveEffect // MP regen
    case (1, 5, 31) => UnknownActiveEffect // entrust no args ("none")
    case (aoeflag, 1, 33) => ImperilEffect.decode(aoeflag == 2, xs)
    case (_, 1, 35) => UnknownActiveEffect // Instance KO arg chance
    case (_, 1, 37) => UnknownActiveEffect // steal, arg bonus chance
    case (_, 2, 38) => UnknownActiveEffect // heal chance, 7 args, last string, 0? 0? chance% heal-amount heal-ratio 5000040? text
    case (aoeflag, 1, 40) => HybridEffect.decode(aoeflag == 2, xs)
    case (_, 1, 41) => UnknownActiveEffect // Fixed damage arg: amount
    case (3, 1, 42) => UnknownActiveEffect // repeated autoattacks args: 0, 0, count-min, count-max, ratio-each
    case (_, 1, 43) => UnknownActiveEffect // crit attack args 0 0 ratio miss-chance
    case (0, 3, 44) => UnknownActiveEffect // dual-black
    case (0, 3, 45) => UnknownActiveEffect // dual cast
    case (_, 1, 47) => UnknownActiveEffect // libra effect arg 134
    case (0, 1, 50) => UnknownActiveEffect // Throw
    case (0, 3, 48) => UnknownActiveEffect // Drink
    case (0, 3, 51) => UnknownActiveEffect // ignorefatal hpthresh chance 0? n-times
    case (1, 1, 52) => UnknownActiveEffect // Jump 0? 0? delay delay ratio
    case (0, 3, 53) => UnknownActiveEffect // hide args min-max
    case (_, 3, 54) => UnknownActiveEffect // dodge physical args: count, turns
    case (_, 2, 56) => UnknownActiveEffect // HealEffect, singing, args: ratio, 1?, base, turns
    case (_, 3, 56) => UnknownActiveEffect // Skip turns, args: _, 1?, _, turns
    case (_, 2, 57) => UnknownActiveEffect // mp refresh, like 2, 56
    case (_, 2, 58) => UnknownActiveEffect // buff effect, singing, 6 args, turns arg5
    case (_, 1, 59) => UnknownActiveEffect // dispel, "none" args
    case (_, 2, 59) => UnknownActiveEffect // ally dispel
    case (0, 3, 61) => UnknownActiveEffect // provoke args: chance turns
    case (0, 3, 64) => UnknownActiveEffect // self-restore pcthp pctmp
    case (aoeflag, 1, 70) => MagicalEffect.decode(aoeflag == 2, xs)
    case (_, 1, 76) => UnknownActiveEffect // steal gil, args low/max pct
    case (_, 1, 81) => UnknownActiveEffect // sacrifice hp -> physical , args 0? 0? 0? 0? 0? 0? ratio pcthp 2000315? 1?
    case (1, 3, 84) => UnknownActiveEffect // sealing blade args 1 0 1 (turns, spells?)
    case (aoeflag, 1, 88) => StopEffect.decode(aoeflag == 2, xs)
    case (0, 3, 101) => UnknownActiveEffect // damage reduction, args pct, turns, 1?
    case (_, 1, 103) => UnknownActiveEffect // spr scaling magic attack, args, spr-mag ratio, max, ratio
    case (_, 1, 105) => UnknownActiveEffect // mp scaling magic attack, args, mp-mag ratio, max, ratio
    case (_, 2, 111) => UnknownActiveEffect // remove debuff from ally, 6 args
  }
}
