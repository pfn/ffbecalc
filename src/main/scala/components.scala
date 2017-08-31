package yaffbedb

import outwatch.dom._
import rxscalajs.{Observable,Subject}

object components {
  def maxstat(unit: Observable[Option[UnitEntry]],
    withPots: Observable[Boolean], f: UnitEntry => StatRange) =
    unit.combineLatest(withPots).map { case (u, p) =>
      u.fold("0")(e => if (p) f(e).maxpots.toString else f(e).max.toString)
    }

  def sortBy(out: outwatch.Sink[Sort]): VNode = {
    val sortAZ = createHandler[Sort](Sort.AZ)
    val sortHP = createHandler[Sort]()
    val sortMP = createHandler[Sort]()
    val sortATK = createHandler[Sort]()
    val sortDEF = createHandler[Sort]()
    val sortMAG = createHandler[Sort]()
    val sortSPR = createHandler[Sort]()
    out <-- sortAZ.merge(sortHP, sortMP, sortATK).merge(sortDEF, sortMAG, sortSPR)
    div(cls := "sort-options", span("Sort"),
      label(input(tpe := "radio", name := "eq-sort", inputChecked(Sort.AZ) --> sortAZ, checked := true), "A-Z"),
      label(input(tpe := "radio", name := "eq-sort", inputChecked(Sort.HP) --> sortHP), "HP"),
      label(input(tpe := "radio", name := "eq-sort", inputChecked(Sort.MP) --> sortMP), "MP"),
      label(input(tpe := "radio", name := "eq-sort", inputChecked(Sort.ATK) --> sortATK), "ATK"),
      label(input(tpe := "radio", name := "eq-sort", inputChecked(Sort.DEF) --> sortDEF), "DEF"),
      label(input(tpe := "radio", name := "eq-sort", inputChecked(Sort.MAG) --> sortMAG), "MAG"),
      label(input(tpe := "radio", name := "eq-sort", inputChecked(Sort.SPR) --> sortSPR), "SPR")),
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
          AilmentResist.zero, ElementResist.zero
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

  def renderResists(resists: List[Int], clz: String) = {
    table(cls := s"unit-resists $clz",
      tr(
        td(div()),
        td(div()),
        td(div()),
        td(div()),
        td(div()),
        td(div()),
        td(div()),
        td(div()),
      ),
      tr(resists.map { r => td(r.toString + "%") }:_*)
    )
  }
  def unitStats(unit: Observable[Option[UnitEntry]], stats: Observable[Option[Stats]], equipped: Observable[(Equipped,Abilities)], allPassives: Observable[SkillEffect.CollatedEffect], esper: Observable[Option[EsperStatInfo]], esperEntry: Observable[Option[EsperEntry]]) = {
    val effective = stats.combineLatest(esper.combineLatest(esperEntry), equipped, allPassives).map {
      case (s,(e,ee),(eqs,abis),pasv) =>
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

          (st * passives + e + eqstats + dhstats ++ ee, passives, pasv.dh, !is2h && isSW)
        }
    }.share
    def st(f: Stats => Int) = effective.map { s =>
      s.fold("???")(d => f(d._1).toString)
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
      children <-- unit.combineLatest(allPassives, effective).map { case (u,pasv,eff) =>
        u.fold(List.empty[VNode]) { entry =>
          List(
            tr(td(colspan := 4,
              renderResists(
                (entry.statusResist + eff.fold(AilmentResist.zero)(_._1.status) + pasv.statusResists.asAilmentResist).asList.map(_._1),
                "status-table")
            )),
            tr(td(colspan := 4,
              renderResists(
                (entry.elementResist + eff.fold(ElementResist.zero)(_._1.element) + pasv.elementResists.asElementResist).asList.map(_._1),
                "elements-table")
            )),
          ) ++
            renderStat(statOf(eff, _.hp), "+HP") ++
            renderStat(statOf(eff, _.mp), "+MP") ++
            renderStat(statOf(eff, _.atk), "+ATK") ++
            renderStat(statOf(eff, _.defs), "+DEF") ++
            renderStat(statOf(eff, _.mag), "+MAG") ++
            renderStat(statOf(eff, _.spr), "+SPR") ++
            renderStat(dhOf(eff, _.hp), "+Equip HP") ++
            renderStat(dhOf(eff, _.mp), "+Equip MP") ++
            renderStat(dhOf(eff, _.atk), "+Equip ATK") ++
            renderStat(dhOf(eff, _.defs), "+Equip DEF") ++
            renderStat(dhOf(eff, _.mag), "+Equip MAG") ++
            renderStat(dhOf(eff, _.spr), "+Equip SPR") ++
            renderStat(statOf(eff, _.crit), "Crit chance", max = 100) ++
            renderDodge(pasv.dodge) ++
            renderKillers(pasv.killers) ++
            renderStat(pasv.lbrate, "+LB fill") ++
            renderStat(pasv.lbfill / 100, "LB/turn", pct = false) ++
            renderStat(pasv.jump, "+Jump Damage") ++
            renderStat(pasv.evomag, "+EVO MAG") ++
            renderStat((eff.fold(0)(_._1.mp) * (pasv.refresh / 100.0)).toInt,
              pasv.refresh + "% MP/turn", pct = false) ++
            renderStat(pasv.attract, "Draw Attacks") ++
            renderStat(pasv.camouflage, "Camouflage")
        }
      }
    )
  }

  def statOf(x: Option[(Stats,PassiveStatEffect,PassiveSinglehandEffect,Boolean)], f: PassiveStatEffect => Int): Int = x.fold(0)(d => f(d._2))
  def dhOf(x: Option[(Stats,PassiveStatEffect,PassiveSinglehandEffect,Boolean)], f: PassiveSinglehandEffect => Int): Int = x.fold(0)(d => if (d._4) f(d._3) else 0)

  def renderStat(stat: Int, label: String, max: Int = 300, pct: Boolean = true): List[VNode] = {
    if (stat != 0) {
      val toohigh = if (stat > max) "; color: red"
      else ""
      List(
        tr(
          td(colspan := 2, label),
          td(colspan := 2, Attributes.style := ("text-align: right" + toohigh), stat + (if (pct) "%" else ""))
        )
      )
    } else Nil
  }

  def renderKillers(killers: Map[Int,(Int,Int)]): List[VNode] = {
    val pkillers = killers.toList.flatMap { case (k,v) =>
      if (v._1 != 0)
        renderStat(v._1, SkillEffect.TRIBE(k) + " Killer")
      else Nil
    }
    val mkillers = killers.toList.flatMap { case (k,v) =>
      if (v._2 != 0)
        renderStat(v._2, "Magic " + SkillEffect.TRIBE(k) + " Killer")
      else Nil
    }
    pkillers ++ mkillers
  }
  def renderDodge(dodge: PassiveDodgeEffect): List[VNode] = {
    renderStat(dodge.phys, "Physical Dodge", max = 100) ++
      renderStat(dodge.mag, "Magical Dodge", max = 100)
  }

  def dataTable[A](data: Seq[A],
    tableCls: String,
    headers: List[String],
    colcls: List[String])(fs: List[A => VNode]): VNode = {
    val rows: List[VNode] =
      tr(headers.zip(colcls).map { case (h, c) => th(cls := c, h) }:_*) ::
        data.toList.map { a =>
          tr(fs.zip(colcls).map { case (f, c) => td(cls := c, f(a)) }: _*)
        }

    table(((cls := tableCls) :: rows): _*)
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
    val a1 = Subject[Option[String]]()
    val a2 = Subject[Option[String]]()
    val a3 = Subject[Option[String]]()
    val a4 = Subject[Option[String]]()
    val ability1Id = createIdHandler(None)
    val ability1 = materiaFor(m, ability1Id.merge(a1)).publishReplay(1).refCount
    val ability2Id = createIdHandler(None)
    val ability2 = materiaFor(m, ability2Id.merge(a2)).publishReplay(1).refCount
    val ability3Id = createIdHandler(None)
    val ability3 = materiaFor(m, ability3Id.merge(a3)).publishReplay(1).refCount
    val ability4Id = createIdHandler(None)
    val ability4 = materiaFor(m, ability4Id.merge(a4)).publishReplay(1).refCount
    (ability1, ability2, ability3, ability4) + 
    unitInfo.combineLatest(unitEntry).map { case (u, e) =>

      val slots = e.fold(0)(_.abilitySlots)

      def materiaList(w: MaybeMateria) = materiaOption(m, up, u, e, sorting, w)
      val m1s = materiaList(ability1)
      val m2s = materiaList(ability2)
      val m3s = materiaList(ability3)
      val m4s = materiaList(ability4)

      if (slots == 0) {
        a1.next(None)
        a2.next(None)
        a3.next(None)
        a4.next(None)
        Nil
      } else if (slots == 1) {
        a2.next(None)
        a3.next(None)
        a4.next(None)
        List(tr(
          mslot("Ability 1", m1s, ability1Id)
        ))
      } else if (slots == 2) {
        a3.next(None)
        a4.next(None)
        List(tr(
          mslot("Ability 1", m1s, ability1Id),
          mslot("Ability 2", m2s, ability2Id)))
      } else if (slots == 3) {
        a4.next(None)
        List(
          tr(
            mslot("Ability 1", m1s, ability1Id),
            mslot("Ability 2", m2s, ability2Id)),
          tr(mslot("Ability 3", m3s, ability3Id)))
      } else {
        List(
          tr(
            mslot("Ability 1", m1s, ability1Id),
            mslot("Ability 2", m2s, ability2Id)),
          tr(
            mslot("Ability 3", m3s, ability3Id),
            mslot("Ability 4", m4s, ability4Id)))
      }
    }
  }

  def mslot(name: String, cs: Observable[List[VNode]], sink: outwatch.Sink[Option[String]]): VNode =
    td(label(name, select(cls := "equip-slot", children <-- cs, inputId --> sink)))

}
