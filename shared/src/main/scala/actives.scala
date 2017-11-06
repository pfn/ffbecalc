package yaffbedb

sealed trait Condition
sealed trait ActiveEffect
object UnknowActiveEffect extends ActiveEffect
trait ConditionalEffect
trait ConditionalBuff
trait DamageEffect
sealed trait HealScaling
case object StandardHealing extends HealScaling
case class HealEffect(ratio: Int, base: Int, turns: Int, aoe: Boolean, scaling: HealScaling = StandardHealing) extends ActiveEffect
object HealEffect {
  def decode(aoe: Boolean, xs: List[Int]) = xs match {
    case List(_, _, c, d, _) =>
      HealEffect(d, c, 1, aoe)
    case List(a, _, c, d) =>
      HealEffect(a, c, d, aoe)
  }
}

object ActiveEffects {
  def apply(x: Int, y: Int, z: Int, xs: List[Int]): ActiveEffect = (x, y, z) match {
    case (aoeflag, 6, 2) => HealEffect.decode(aoeflag == 2, xs)
    case (aoeflag, 2, 8) => HealEffect.decode(aoeflag == 2, xs)
    case _ => UnknowActiveEffect
  }
}
