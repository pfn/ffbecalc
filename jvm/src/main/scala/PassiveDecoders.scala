package com.ffbecalc
import io.circe._
import optics.JsonPath.root

object PassiveDecoders {
  def apply(restrict: List[Int], x: Int, y: Int, effect: Int, c: Option[Json]): SkillEffect =
    (effect,ActiveDecoders.listInt(c)) match {
      case (11,Nil) =>
        val tribes = c.toList.flatMap(root(0).each.int.getAll)
        val phys   = c.toList.flatMap(root(1).each.int.getAll)
        val mag    = c.toList.flatMap(root(2).each.int.getAll)
        PassiveKillersEffect(tribes.zip(phys.zip(mag)).toMap)
      case (_,xs) => SkillEffect(restrict, x, y, effect, xs)
    }
}
