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
    val hpCheck  = createBoolHandler(true)
    val mpCheck  = createBoolHandler(true)
    val atkCheck = createBoolHandler(true)
    val defCheck = createBoolHandler(true)
    val magCheck = createBoolHandler(true)
    val sprCheck = createBoolHandler(true)

    stats <-- unit.combineLatest(hpCheck.combineLatest(mpCheck).combineLatest(atkCheck.combineLatest(defCheck, magCheck, sprCheck))).map {
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
          child <-- maxstat(unit, hpCheck, _.stats.hp)),
        td(cls := "unit-stat-name",
          input(id := "mp-pot-check", tpe := "checkbox", checked := true, inputChecked --> mpCheck),
          label(forLabel := "mp-pot-check", "MP")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, mpCheck, _.stats.mp)),
      ),
      tr(
        td(cls := "unit-stat-name",
          input(id := "atk-pot-check", tpe := "checkbox", checked := true, inputChecked --> atkCheck),
          label(forLabel := "atk-pot-check", "ATK")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, atkCheck, _.stats.atk)),
        td(cls := "unit-stat-name",
          input(id := "def-pot-check", tpe := "checkbox", checked := true, inputChecked --> defCheck),
          label(forLabel := "def-pot-check", "DEF")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, defCheck, _.stats.defs))
      ),
      tr(
        td(cls := "unit-stat-name",
          input(id := "mag-pot-check", tpe := "checkbox", checked := true, inputChecked --> magCheck),
          label(forLabel := "mag-pot-check", "MAG")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, magCheck, _.stats.mag)),
        td(cls := "unit-stat-name",
          input(id := "spr-pot-check", tpe := "checkbox", checked := true, inputChecked --> sprCheck),
          label(forLabel := "spr-pot-check", "SPR")),
        td(cls := "unit-stat-data",
          child <-- maxstat(unit, sprCheck, _.stats.spr))
      ),
    )
  }

  def unitStats(unit: Observable[Option[UnitEntry]], stats: Observable[Option[Stats]], equipped: Observable[(Equipped,Abilities)], allPassives: Observable[SkillEffect.CollatedEffect], esper: Observable[Option[EsperStatInfo]]) = {
    val effective = stats.combineLatest(esper, equipped, allPassives).map {
      case (s,e,(eqs,abis),pasv) =>
        s.map { st =>
          val alleq = eqs.allEquipped
          val passives = (pasv.stats + pasv.statFromEquips(alleq))

          val is2h = alleq.exists(_.twohands)
          val isSW = alleq.count(_.slotId == 1) == 1 &&
            alleq.count(_.slotId == 2) == 0
          val eqstats = alleq.foldLeft(Stats.zero) { (ac, equip) =>
            ac + equip.stats
          }
          val dhstats = if (!is2h && isSW) eqstats * pasv.dh
          else if (is2h && isSW) Stats.zero
          else Stats.zero

          st * passives + e + eqstats + dhstats
        }
    }
    def st(f: Stats => Int) = effective.map { s =>
      s.fold("???")(d => f(d).toString)
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
      /*
      children <-- unit.combineLatest(allPassives).map { case (u,pasv) =>
        u.fold(List.empty[VNode]) { entry =>
          List(
            tr(
              td(colspan := 1, "Status Resist"),
              td(colspan := 3, entry.statusResist.toString),
            ),
            tr(
              td(colspan := 1, "Element Resist"),
              td(colspan := 3, entry.elementResist.toString),
            ),
            tr(
              td(colspan := 1, "Passive Eleres"),
              td(colspan := 3, pasv.elementResists.toString),
            ),
            tr(
              td(colspan := 1, "Passive ailres"),
              td(colspan := 3, pasv.statusResists.toString),
            ),
            tr(
              td(colspan := 1, "Killers"),
              td(colspan := 3, pasv.killers.toString),
            ),
            tr(
              td(colspan := 1, "EVO MAG %"),
              td(colspan := 3, pasv.evomag.toString),
            ),
            tr(
              td(colspan := 1, "Dodge %"),
              td(colspan := 3, pasv.dodge.toString),
            ),
            tr(
              td(colspan := 1, "Jump"),
              td(colspan := 3, pasv.jump.toString),
            ),
            tr(
              td(colspan := 1, "LB %"),
              td(colspan := 3, pasv.lbrate.toString),
            ),
            tr(
              td(colspan := 1, "LB fill"),
              td(colspan := 3, (pasv.lbfill / 100.0).toString),
            ),
            tr(
              td(colspan := 1, "Auto-refresh"),
              td(colspan := 3, pasv.refresh.toString),
            ),
            tr(
              td(colspan := 1, "Camouflage"),
              td(colspan := 3, pasv.camouflage.toString),
            ),
          )
        }
      }
      */
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

  def effectiveStats(u: UnitData, equip: MateriaIndex, pasv: SkillEffect.CollatedEffect): PassiveStatEffect = {
    val innates = SkillEffect.collateEffects(Some(u), equip.skilleffects)

    val innatestats = innates.stats :: innates.equipStats.keys.toList.flatMap {
      k => if (pasv.canEquip(k, Some(u))) List(innates.equipStats(k)) else Nil
    }
    innatestats.foldLeft(PassiveStatEffect.zero) { (ac, x) =>
      ac + x
    }
  }
  def sortFor(xs: List[MateriaIndex], sorting: Sort, pasv: SkillEffect.CollatedEffect, unit: Option[UnitData]) = {
    val m = for {
      u <- unit
    } yield {
      val es = effectiveStats(u, _: MateriaIndex, pasv)
      def cmp(f: PassiveStatEffect => Int):
        (MateriaIndex,MateriaIndex) => Boolean = (x,y) => f(es(x)) > f(es(y))
        
      val f: (MateriaIndex,MateriaIndex) => Boolean = sorting match {
        case Sort.AZ  => (_,_) => true
        case Sort.HP  => cmp(_.hp)
        case Sort.MP  => cmp(_.mp)
        case Sort.ATK => cmp(_.atk)
        case Sort.DEF => cmp(_.defs)
        case Sort.MAG => cmp(_.mag)
        case Sort.SPR => cmp(_.spr)
      }

      if (sorting == Sort.AZ) xs else xs.sortWith(f)
    }
    m.getOrElse(xs)
  }
  def materiaOption(ms: Observable[List[MateriaIndex]], up: Observable[Seq[SkillEffect]], u: Option[UnitData], e: Option[UnitEntry], sorting: Observable[Sort], worn: Observable[Option[MateriaIndex]]): Observable[List[VNode]] = ms.combineLatest(up, sorting, worn).map {
    case (m, ps, s, w) =>
      val mats = m.filter(mi => e.exists(_.canEquip(mi)))
      List(option(value := EMPTY, "Empty")) ++
        sortFor(mats, s, SkillEffect.collateEffects(u, ps.toList), u).map { mi =>
          val mid = mi.describeEffects(u)
          val mids = if (mid.trim.isEmpty) ""
          else s"\u27a1 $mid"
          option(value := mi.id,
            selected := w.exists(_.id == mi.id),
            s"${mi.name} $mids")
        }
    }
  def materiaFor(m: Observable[List[MateriaIndex]], idOb: Observable[Option[String]]): Observable[Option[MateriaIndex]] = for {
    ms <- m
    id <- idOb
  } yield ms.find(_.id == id.flatMap(i => util.Try(i.toInt).toOption).getOrElse(0))
  type MaybeMateria = Observable[Option[MateriaIndex]]
  def abilitySlots(m: Observable[List[MateriaIndex]], unitInfo: Observable[Option[UnitData]], up: Observable[Seq[SkillEffect]], unitEntry: Observable[Option[UnitEntry]], sorting: Observable[Sort]): (MaybeMateria,MaybeMateria,MaybeMateria,MaybeMateria,Observable[List[VNode]]) = {
    val ability1Id = createIdHandler(None)
    val ability1 = materiaFor(m, ability1Id)
    val ability2Id = createIdHandler(None)
    val ability2 = materiaFor(m, ability2Id)
    val ability3Id = createIdHandler(None)
    val ability3 = materiaFor(m, ability3Id)
    val ability4Id = createIdHandler(None)
    val ability4 = materiaFor(m, ability4Id)
    (ability1, ability2, ability3, ability4) + 
    unitInfo.combineLatest(unitEntry).map { case (u, e) =>

      val slots = e.fold(0)(_.abilitySlots)

      def materiaList(w: MaybeMateria) = materiaOption(m, up, u, e, sorting, w)
      val m1s = materiaList(ability1)
      val m2s = materiaList(ability2)
      val m3s = materiaList(ability3)
      val m4s = materiaList(ability4)

      if (slots == 0) {
        Nil
      } else if (slots == 1) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- m1s, inputId --> ability1Id))))
      } else if (slots == 2) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- m1s, inputId --> ability1Id)),
          td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- m2s, inputId --> ability2Id))))
      } else if (slots == 3) {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- m1s, inputId --> ability1Id)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- m2s, inputId --> ability2Id))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- m3s, inputId --> ability3Id))))
      } else {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- m1s, inputId --> ability1Id)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- m2s, inputId --> ability2Id))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- m3s, inputId --> ability3Id)),
            td(label(forLabel := "u-ability4", "Ability 4"), select(id := "u-ability4", cls := "equip-slot", children <-- m4s, inputId --> ability4Id))))
      }
    }
  }

}
