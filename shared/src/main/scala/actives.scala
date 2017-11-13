package yaffbedb

sealed trait Target
sealed trait TargetClass
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
sealed trait ActiveEffect
sealed trait AttackEffect {
  def elements: List[String]
}
object ActiveEffect {
  def unapply(xs: (Int, Int, Int)): Option[(SkillTarget, Int)] = xs match { case (x, y, z) =>
    val tgt = x match {
      case 0 => Some(Target.Self)
      case 1 => Some(Target.Single)
      case 2 => Some(Target.AoE)
      case 3 => Some(Target.Random)
      case _ => None
    }

    val cls = y match {
      case 1 => Some(Target.Enemy)
      case 2 => Some(Target.Party)
      case 3 => Some(Target.Self)
      case 4 => Some(Target.All)
      case 5 => Some(Target.Allies)
      case 6 => Some(Target.KOd)
      case _ => None
    }

    for {
      t <- tgt
      c <- cls
    } yield (SkillTarget(t, c), z)
  }
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

  // repeated count = range min-max
  def apply(x: Int, y: Int, z: Int, xs: List[Int]): ActiveEffect = (x, y, z) match {
    case (aoeflag, _, 1) => PhysicalEffect.decode(aoeflag == 2, xs)
    case (aoeflag, _, 2) => HealEffect.decode(aoeflag == 2, xs)
    case (_, _, 3) => UnknownActiveEffect // BuffEffect.decode(tgt, xs)
    case (_, _, 4) => UnknownActiveEffect // raise effect, args pct heal
    case (_, _, 5) => UnknownActiveEffect // ailments cure, args 1 2 3 4 5 6 7 8 (or 0 for not cure)
    case (_, _, 6) => UnknownActiveEffect // StatusAilmentEffect, args ailment-pcts 1?
    case (_, _, 7) => UnknownActiveEffect // status ailment resist, args: [ailment chances 1? turns]
    case (aoeflag, _, 8) => HealEffect.decode(aoeflag == 2, xs) // hp regen
    case (_, _, 9) => UnknownActiveEffect // pcthp damage, pct pct 100?
    case (_, 1, 10) => UnknownActiveEffect // physical osmose drainratio ratio 100?
    case (_, 5, 11) => UnknownActiveEffect // sacrifice self, restore pct, args [pcthp pctmp]
    case (_, 1, 13) => UnknownActiveEffect // delay damage, args: delay 0? 0? 2? ??? ratio
    case (aoeflag, 1, 15) => MagicalEffect.decode(aoeflag == 2, xs)
    case (_, _, 16) => UnknownActiveEffect // restore hp, arg amount
    case (_, _, 17) => UnknownActiveEffect // mp heal, self? args N
    case (_, _, 18) => UnknownActiveEffect // reduce physical damage, args: pct turns 1?
    case (_, _, 19) => UnknownActiveEffect // reduce magic damage, args: pct turns 1? // mag is broken, is inverse break
    case (_, 3, 20) => UnknownActiveEffect // store +ratio maxratio 0?
    case (aoeflag, _, 21) => PhysicalEffect.decode(aoeflag == 2, xs) // ignore cover
    case (_, _, 22) => UnknownActiveEffect // phys damage, bonus to type, args: type 0? 0? bonus-ratio
    case (_, 1, 23) => UnknownActiveEffect // magic damage, bonus to type, args: type 0? 0? bonus-ratio
    case (aoeflag, 1, 24) => DebuffEffect.decode(aoeflag == 2, xs)
    case (_, _, 24) => UnknownActiveEffect // stat debuff
    case (_, 1, 25) => UnknownActiveEffect // physical drain drainratio ratio 100?
    case (_, _, 26) => UnknownActiveEffect // restore hp, args restore% restore% 100?
    case (_, _, 27) => UnknownActiveEffect // reraise, args pct, turns
    case (_, 2, 28) => UnknownActiveEffect // salve, args = valid item ids
    case (_, _, 29) => UnknownActiveEffect // random skill args: [[skill, pct]]
    case (_, _, 30) => UnknownActiveEffect // MP regen, args: scale, 1?, base, turns
    case (1, 5, 31) => UnknownActiveEffect // entrust no args ("none")
    case (0, 3, 32) => UnknownActiveEffect // fill esper gauge, args range(min max)
    case (aoeflag, _, 33) => ImperilEffect.decode(aoeflag == 2, xs) // also resists
    case (_, _, 34) => UnknownActiveEffect // inflict random ailment, args [chances chances... 1 ailmentcount]
    case (_, _, 35) => UnknownActiveEffect // Instance KO arg chance
    case (_, 1, 37) => UnknownActiveEffect // steal, arg bonus chance
    case (_, 2, 38) => UnknownActiveEffect // heal chance, 7 args, last string, 0? 0? chance% heal-amount heal-ratio 5000040? text
    case (_, 1, 39) => UnknownActiveEffect // sacrifice %hp to deal %hp, args: 1? sac-pct 2? dmg-pct 1? 1?
    case (aoeflag, 1, 40) => HybridEffect.decode(aoeflag == 2, xs)
    case (_, _, 41) => UnknownActiveEffect // Fixed damage arg: amount
    case (_, 1, 42) => UnknownActiveEffect // repeated autoattacks args: 0, 0, count-min, count-max, ratio-each
    case (_, 1, 43) => UnknownActiveEffect // crit attack args 0 0 ratio miss-chance
    case (0, 3, 44) => UnknownActiveEffect // dual-black
    case (0, 3, 45) => UnknownActiveEffect // dual cast
    case (_, 1, 47) => UnknownActiveEffect // libra effect arg 134
    case (0, 1, 50) => UnknownActiveEffect // Throw
    case (0, 3, 48) => UnknownActiveEffect // Drink
    case (_, _, 51) => UnknownActiveEffect // escape battle args, pct +wtf?
    case (1, 1, 52) => UnknownActiveEffect // Jump 0? 0? delay delay ratio
    case (0, 3, 52) => UnknownActiveEffect // dual specific magic, args: 2? count skill-id
    case (0, 3, 53) => UnknownActiveEffect // hide args min-max
    case (_, _, 54) => UnknownActiveEffect // dodge physical args: count, turns
    case (_, 2, 56) => UnknownActiveEffect // HealEffect, singing, args: ratio, 1?, base, turns
    case (_, 3, 56) => UnknownActiveEffect // Skip turns, args: _, 1?, _, turns
    case (_, 2, 57) => UnknownActiveEffect // mp refresh, like 2, 56
    case (_, 2, 58) => UnknownActiveEffect // buff effect, singing, 6 args, turns arg5
    case (_, _, 59) => UnknownActiveEffect // dispel, "none" args
    case (_, 1, 60) => UnknownActiveEffect // charm args: turns, pct, string
    case (_, _, 61) => UnknownActiveEffect // provoke args: chance turns
    case (_, _, 63) => UnknownActiveEffect // lbfill rate+ args: pct, turns
    case (_, _, 64) => UnknownActiveEffect // restore pcthp pctmp
    case (_, _, 65) => UnknownActiveEffect // restore hp/mp, args: hp mp
    case (1, 2, 66) => UnknownActiveEffect // giants drink, [+hp% +mp% turns]
    case (_, _, 68) => UnknownActiveEffect // inflict berserk, args turns atkbuff
    case (aoeflag, _, 70) => MagicalEffect.decode(aoeflag == 2, xs) // ignore cover
    case (0, 3, 71) => UnknownActiveEffect // invoke skill, args: skill-id 1?:w
    case (_, 1, 72) => UnknownActiveEffect // stacking magic damage, args: 0? 0? init-0? base-ratio stack-ratio maxcount
    case (_, 1, 76) => UnknownActiveEffect // steal gil, args low/max pct
    case (_, _, 81) => UnknownActiveEffect // sacrifice hp -> physical , args 0? 0? 0? 0? 0? 0? ratio pcthp 2000315? 1?
    case (0, 0, 82) => UnknownActiveEffect // random magic, args [[skill, pct]]
    case (_, _, 83) => UnknownActiveEffect // set hp to arg
    case (1, 3, 84) => UnknownActiveEffect // sealing blade args 1 0 1 (turns, spells?)
    case (_, _, 86) => UnknownActiveEffect // reflect spells, args: 100? count turns
    case (aoeflag, 1, 88) => StopEffect.decode(aoeflag == 2, xs)
    case (_, _, 89) => UnknownActiveEffect // debuff resistance, args: atk def mag spr stop charm? turns 1?
    case (0, 3, 90) => UnknownActiveEffect // mag store, args ratio maxratio turns?
    case (_, _, 92) => UnknownActiveEffect // add physical killer effect, as mag killer below
    case (_, _, 93) => UnknownActiveEffect // add magic killer effect, args [race pct] -1 -1 -1 -1 -1 -1 -1 turns 1?
    case (_, _, 95) => UnknownActiveEffect // imbue element, args element-list-imbuepct turns 1?
    case (_, _, 96) => UnknownActiveEffect // aoe phys cover, args 1? 0? preduce mreduce chance 100? turns 1? 1?
    case (0, 3, 97) => UnknownActiveEffect // unlock skill args, 2? 2? skill-id turns max?
    case (0, 3, 98) => UnknownActiveEffect // unlock skill args, use-count? skill-id-old?? 1? [skill-ids] count turns 0?
    case (_, _, 99) => UnknownActiveEffect // conditional trigger, args 2? trigger-skill 2? true-skill 2? false-skill [2 = turns?]
                                           // alternate, args = [2? 2? 2?] [skill-trigger1 skill-trigger2 skill-trigger3] 2 true-skill 2 false-skill
    case (_, _, 100) => UnknownActiveEffect // unlock skill, args 1? skill-id use-count? turns-count 1?
                                            // alternate, args = [2 2] [skill-id skill-id] use-count? turns 1?
    case (_, _, 101) => UnknownActiveEffect // damage reduction, args pct, turns, 1?
    case (_, 1, 103) => UnknownActiveEffect // spr scaling magic attack, args, spr-mag ratio, max, ratio
    case (_, 1, 105) => UnknownActiveEffect // mp scaling magic attack, args, mp-mag ratio, max, ratio
    case (_, _, 111) => UnknownActiveEffect // remove debuff from ally, 6 args (atk, def, mag, spr, stop? charm?)
    case (_, _, 112) => UnknownActiveEffect // damage or death, args: ratio deathpct atkpct 0?
    case (_, _, 118) => UnknownActiveEffect // targeted cover, args: preduce mreduce pct? turns 1? 0?
    case (0, 3, 119) => UnknownActiveEffect // active counter, args: pct scaling-stat(1=atk,3=mag) turns max-per-turn?
    case (0, 3, 122) => UnknownActiveEffect // grant death immunity, args: turns 0?
    case (0, 3, 123) => UnknownActiveEffect // active counter, args: pct scaling-stat(1=atk,3=mag) turns max-per-turn?
    case (_, _, 124) => UnknownActiveEffect // hybrid evoke gauge damage , args 0 0 0 0 0 0 0 mag spr-ratios [50? 50?]
    case (_, _, 125) => UnknownActiveEffect // increase LB gauge args: min max
    case (1, 6, 1002) => UnknownActiveEffect // survivor flask, args: turns 9?
    case (0, 3, 1003) => UnknownActiveEffect // fill LB gauge, args 0? pct
    case (1, 1, 1005) => UnknownActiveEffect // twist of fate, args: 2? 2? 100? "xx;yy;..."
    case (0, 3, 1006) => UnknownActiveEffect // GE dual-ability, args count [skill-id, ...]
    case (0, 3, 1011) => UnknownActiveEffect // skip turns, args: count
    case ActiveEffect(t, active) => active match {
      case 1 => UnknownActiveEffect
    }
  }
}
