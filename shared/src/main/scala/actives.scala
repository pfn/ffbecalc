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
  def apply(x: Int, y: Int, z: Int, xs: List[Int]): ActiveEffect = (x, y, z) match {
    case (aoeflag, 1, 1) => PhysicalEffect.decode(aoeflag == 2, xs)
    case (aoeflag, 6, 2) => HealEffect.decode(aoeflag == 2, xs)
    case (_, 6, 4) => UnknownActiveEffect // raise effect, args pct heal
    case (_, 3, 3) => UnknownActiveEffect // BuffEffect.decode(tgt, xs)
    case (_, 2, 5) => UnknownActiveEffect // ally status ailments cure, args 1 2 3 4 5 6 7 8
    case (_, 1, 6) => UnknownActiveEffect // StatusAilmentEffect
    case (aoeflag, 2, 8) => HealEffect.decode(aoeflag == 2, xs)
    case (aoeflag, 1, 15) => MagicalEffect.decode(aoeflag == 2, xs)
    case (_, 3, 17) => UnknownActiveEffect // mp heal, self? args N
    case (aoeflag, 1, 21) => PhysicalEffect.decode(aoeflag == 2, xs)
    case (aoeflag, 1, 24) => DebuffEffect.decode(aoeflag == 2, xs)
    case (_, 2, 27) => UnknownActiveEffect // reraise, args pct, turns
    case (_, 2, 30) => UnknownActiveEffect // MP regen, args: scale, 1?, base, turns
    case (_, 3, 30) => UnknownActiveEffect // MP regen
    case (aoeflag, 1, 33) => ImperilEffect.decode(aoeflag == 2, xs)
    case (_, 1, 37) => UnknownActiveEffect // steal, arg bonus chance
    case (aoeflag, 1, 40) => HybridEffect.decode(aoeflag == 2, xs)
    case (_, 1, 47) => UnknownActiveEffect // libra effect arg 134
    case (_, 3, 54) => UnknownActiveEffect // dodge physical args: count, turns
    case (_, 2, 56) => UnknownActiveEffect // HealEffect, singing, args: ratio, 1?, base, turns
    case (_, 3, 56) => UnknownActiveEffect // Skip turns, args: _, 1?, _, turns
    case (_, 2, 57) => UnknownActiveEffect // mp refresh, like 2, 56
    case (_, 2, 58) => UnknownActiveEffect // buff effect, singing, 6 args, turns arg5
    case (_, 1, 59) => UnknownActiveEffect // dispel, "none" args
    case (_, 2, 59) => UnknownActiveEffect // ally dispel
    case (aoeflag, 1, 70) => MagicalEffect.decode(aoeflag == 2, xs)
    case (_, 1, 76) => UnknownActiveEffect // steal gil, args low/max pct
    case (aoeflag, 1, 88) => StopEffect.decode(aoeflag == 2, xs)
    case (_, 1, 103) => UnknownActiveEffect // spr scaling magic attack, args, spr-mag ratio, max, ratio
    case (_, 1, 105) => UnknownActiveEffect // mp scaling magic attack, args, mp-mag ratio, max, ratio
    case (_, 2, 111) => UnknownActiveEffect // remove debuff from ally, 6 args
  }
}
