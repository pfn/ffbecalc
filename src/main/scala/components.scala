package yaffbedb

import outwatch.dom._
import rxscalajs.Observable

object components {
  def maxstat(unit: Observable[Option[UnitEntry]],
    withPots: Observable[Boolean], f: UnitEntry => StatRange) =
    unit.combineLatest(withPots).map { case (u, p) =>
      u.fold("0")(e => if (p) f(e).maxpots.toString else f(e).max.toString)
    }

  def unitStats(unit: Observable[Option[UnitEntry]]): VNode = {
    val hpCheck  = createBoolHandler()
    val mpCheck  = createBoolHandler()
    val atkCheck = createBoolHandler()
    val defCheck = createBoolHandler()
    val magCheck = createBoolHandler()
    val sprCheck = createBoolHandler()
    table(
      tr(
        td(cls := "unit-stat-name",
          input(id := "hp-pot-check", tpe := "checkbox", checked := true, inputChecked --> hpCheck),
          label(forLabel := "hp-pot-check", "HP")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, hpCheck.startWith(true), _.stats.hp)),
        td(cls := "unit-stat-name",
          input(id := "mp-pot-check", tpe := "checkbox", checked := true, inputChecked --> mpCheck),
          label(forLabel := "mp-pot-check", "MP")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, mpCheck.startWith(true), _.stats.mp)),
      ),
      tr(
        td(cls := "unit-stat-name",
          input(id := "atk-pot-check", tpe := "checkbox", checked := true, inputChecked --> atkCheck),
          label(forLabel := "atk-pot-check", "ATK")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, atkCheck.startWith(true), _.stats.atk)),
        td(cls := "unit-stat-name",
          input(id := "def-pot-check", tpe := "checkbox", checked := true, inputChecked --> defCheck),
          label(forLabel := "def-pot-check", "DEF")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, defCheck.startWith(true), _.stats.defs))
      ),
      tr(
        td(cls := "unit-stat-name",
          input(id := "mag-pot-check", tpe := "checkbox", checked := true, inputChecked --> magCheck),
          label(forLabel := "mag-pot-check", "MAG")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, magCheck.startWith(true), _.stats.mag)),
        td(cls := "unit-stat-name",
          input(id := "spr-pot-check", tpe := "checkbox", checked := true, inputChecked --> sprCheck),
          label(forLabel := "spr-pot-check", "SPR")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, sprCheck.startWith(true), _.stats.spr))
      ),
    )
  }
}
