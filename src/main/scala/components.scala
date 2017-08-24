package yaffbedb

import outwatch.dom._
import rxscalajs.Observable

object components {
  def maxstat(unit: Observable[Option[UnitEntry]],
    withPots: Observable[Boolean], f: UnitEntry => StatRange) =
    unit.combineLatest(withPots).map { case (u, p) =>
      u.fold("0")(e => if (p) f(e).maxpots.toString else f(e).max.toString)
    }

  def unitBaseStats(unit: Observable[Option[UnitEntry]], stats: outwatch.Sink[Option[Stats]]): VNode = {
    val hpCheck  = createBoolHandler()
    val mpCheck  = createBoolHandler()
    val atkCheck = createBoolHandler()
    val defCheck = createBoolHandler()
    val magCheck = createBoolHandler()
    val sprCheck = createBoolHandler()

    stats <-- unit.combineLatest(hpCheck.startWith(true).combineLatest(mpCheck.startWith(true)).combineLatest(atkCheck.startWith(true).combineLatest(defCheck.startWith(true), magCheck.startWith(true), sprCheck.startWith(true)))).map {
      case (entry, ((hp,mp),(atk,defs,mag,spr))) =>
      entry.map { e =>
        Stats(
          if (hp)   e.stats.hp.maxpots   else e.stats.hp.max,
          if (mp)   e.stats.mp.maxpots   else e.stats.mp.max,
          if (atk)  e.stats.atk.maxpots  else e.stats.atk.max,
          if (defs) e.stats.defs.maxpots else e.stats.defs.max,
          if (mag)  e.stats.mag.maxpots  else e.stats.mag.max,
          if (spr)  e.stats.spr.maxpots  else e.stats.spr.max,
        )
      }
    }
    table(cls := "unit-stats",
      caption("Base Stats"),
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

  def unitStats(stats: Observable[Option[Stats]], esper: Observable[Option[EsperStatInfo]]): VNode = {
    def st(f: Stats => Int) = stats.combineLatest(esper).map { case (s,e) =>
      s.fold("???")(d => f(d + e).toString)
    }
    table(cls := "unit-stats",
      caption("Effective Stats"),
      tr(
        td(cls := "unit-stat-name", "HP"),
        td(cls := "unit-stat-data", child <-- st(_.hp)),
        td(cls := "unit-stat-name", "MP"),
        td(cls := "unit-stat-data", child <-- st(_.mp)),
      ),
      tr(
        td(cls := "unit-stat-name", "ATK"),
        td(cls := "unit-stat-data", child <-- st(_.atk)),
        td(cls := "unit-stat-name", "DEF"),
        td(cls := "unit-stat-data", child <-- st(_.defs))
      ),
      tr(
        td(cls := "unit-stat-name", "MAG"),
        td(cls := "unit-stat-data", child <-- st(_.mag)),
        td(cls := "unit-stat-name", "SPR"),
        td(cls := "unit-stat-data", child <-- st(_.spr))
      ),
    )
  }

  def dataTable[A](data: Observable[Seq[A]],
    tableCls: String,
    headers: List[String],
    colcls: List[String])(fs: List[A => VNode]): VNode = {
    table(cls := tableCls,
      tr(headers.zip(colcls).map { case (h, c) =>
          th(cls := c, h);
        }: _*), children <-- data.map { d =>
          d.map { a =>
            tr(fs.zip(colcls).map { case (f, c) =>
              td(cls := c, f(a))
            }: _*)
          }
        }
    )
  }

  def materiaOption(ms: Observable[List[MateriaIndex]], u: Option[UnitData], e: Option[UnitEntry]): Observable[List[VNode]] =
    ms.map { m =>
      List(option(value := EMPTY, "Empty")) ++
        m.filter(mi => e.exists(_.canEquip(mi))).map { mi =>
          val mid = mi.describeEffects(u)
          val mids = if (mid.trim.isEmpty) ""
          else s"\u27a1 $mid"
          option(value := mi.id, s"${mi.name} $mids")
        }
    }
  def materiaFor(m: Observable[List[MateriaIndex]], idOb: Observable[Option[String]]): Observable[Option[MateriaIndex]] = for {
    ms <- m
    id <- idOb
  } yield ms.find(_.id == id.flatMap(i => util.Try(i.toInt).toOption).getOrElse(0))
  type MaybeMateria = Observable[Option[MateriaIndex]]
  def abilitySlots(m: Observable[List[MateriaIndex]], unitInfo: Observable[Option[UnitData]], unitEntry: Observable[Option[UnitEntry]]): (MaybeMateria,MaybeMateria,MaybeMateria,MaybeMateria,Observable[List[VNode]]) = {
    val ability1Sink = createStringHandler()
    val ability1Id = prependNone(ability1Sink)
    val ability1 = materiaFor(m, ability1Id)
    val ability2Sink = createStringHandler()
    val ability2Id = prependNone(ability2Sink)
    val ability2 = materiaFor(m, ability2Id)
    val ability3Sink = createStringHandler()
    val ability3Id = prependNone(ability3Sink)
    val ability3 = materiaFor(m, ability3Id)
    val ability4Sink = createStringHandler()
    val ability4Id = prependNone(ability4Sink)
    val ability4 = materiaFor(m, ability4Id)
    (ability1, ability2, ability3, ability4) + 
    unitInfo.combineLatest(unitEntry).map { case (u, e) =>

      val slots = e.fold(0)(_.abilitySlots)

      if (slots == 0) {
        Nil
      } else if (slots == 1) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability1Sink))))
      } else if (slots == 2) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability1Sink)),
          td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability2Sink))))
      } else if (slots == 3) {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability1Sink)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability2Sink))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability3Sink))))
      } else {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability1Sink)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability2Sink))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability3Sink)),
            td(label(forLabel := "u-ability4", "Ability 4"), select(id := "u-ability4", cls := "equip-slot", children <-- materiaOption(m, u, e), inputString --> ability4Sink))))
      }
    }
  }

}
