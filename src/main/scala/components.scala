package yaffbedb

import outwatch.dom._
import rxscalajs.{Observable,Subject}

object components {

  def sortBy(out: outwatch.Sink[Sort]): VNode = {
    val sortAZ = createHandler[Sort](Sort.AZ)
    val sortHP = createHandler[Sort]()
    val sortMP = createHandler[Sort]()
    val sortATK = createHandler[Sort]()
    val sortDEF = createHandler[Sort]()
    val sortMAG = createHandler[Sort]()
    val sortSPR = createHandler[Sort]()
    out <-- sortAZ.merge(sortHP, sortMP, sortATK).merge(sortDEF, sortMAG, sortSPR)

    def sortItem(s: Sort, h: Handler[Sort], n: String, check: Boolean = false) =
      label(input(tpe := "radio", name := "eq-sort",
        inputChecked(s) --> h, checked := true), n)

    div(cls := "sort-options", span("Sort"),
      sortItem(Sort.AZ,  sortAZ,  "A-Z", true),
      sortItem(Sort.HP,  sortHP,  "HP"),
      sortItem(Sort.MP,  sortMP,  "MP"),
      sortItem(Sort.ATK, sortATK, "ATK"),
      sortItem(Sort.DEF, sortDEF, "DEF"),
      sortItem(Sort.MAG, sortMAG, "MAG"),
      sortItem(Sort.SPR, sortSPR, "SPR"))
  }
  def unitBaseStats(unit: Observable[Option[UnitEntry]], stats: outwatch.Sink[Option[BaseStats]], sub: PotSubjects) = {
    def potsFor(unit: Option[UnitEntry], f: StatInfo => StatRange): Int =
      unit.fold(0)(e => f(e.stats).pots)
    def maxstat(unit: Option[UnitEntry],
      withPots: Observable[Int], f: StatInfo => StatRange) =
      withPots.map { p =>
        unit.fold(0)(e => f(e.stats).max + p).toString
      }

    unit.map { u =>
      val potClicks = createHandler[Unit](())
      val showPots = potClicks.scan(false) { (show,_) => !show }
      def createIntHandler(x: Int) = createHandler[Int](x)
      val hpPots  = createIntHandler(potsFor(u, _.hp))
      val mpPots  = createIntHandler(potsFor(u, _.mp))
      val atkPots = createIntHandler(potsFor(u, _.atk))
      val defPots = createIntHandler(potsFor(u, _.defs))
      val magPots = createIntHandler(potsFor(u, _.mag))
      val sprPots = createIntHandler(potsFor(u, _.spr))
      stats <-- unit.combineLatest(hpPots.merge(sub.hp).combineLatest(mpPots.merge(sub.mp)).combineLatest(atkPots.merge(sub.atk).combineLatest(defPots.merge(sub.defs), magPots.merge(sub.mag), sprPots.merge(sub.spr)))).map {
        case (entry, ((hp,mp),(atk,defs,mag,spr))) =>
        entry.map { e =>
          BaseStats(
            e.stats.hp.max + hp,
            e.stats.mp.max + mp,
            e.stats.atk.max + atk,
            e.stats.defs.max + defs,
            e.stats.mag.max + mag,
            e.stats.spr.max + spr,
            Pots(hp, mp, atk, defs, mag, spr)
          )
        }
      }
      div(cls := "unit-stats",
        div(
          div("HP " , child <-- maxstat(u, hpPots, _.hp)),
          div("MP " , child <-- maxstat(u, mpPots, _.mp)),
          div("ATK ", child <-- maxstat(u, atkPots, _.atk)),
          div("DEF ", child <-- maxstat(u, defPots, _.defs)),
          div("MAG ", child <-- maxstat(u, magPots, _.mag)),
          div("SPR ", child <-- maxstat(u, sprPots, _.spr)),
          button("Pots", tpe := "button", click(()) --> potClicks),
        ),
        div(hidden <-- showPots,
          potSlider("HP",  potsFor(u, _.hp),  10,  hpPots),
          potSlider("MP",  potsFor(u, _.mp),   5,  mpPots),
          potSlider("ATK", potsFor(u, _.atk),  1,  atkPots),
          potSlider("DEF", potsFor(u, _.defs), 1,  defPots),
          potSlider("MAG", potsFor(u, _.mag),  1,  magPots),
          potSlider("SPR", potsFor(u, _.spr),  1,  sprPots),
        ),
      )
    }
  }

  val inputInt = inputNumber(_.toInt)
  def potSlider(name: String, maxv: Int, steps: Int, h: Handler[Int]) = {
    div(cls := "pot-slider", 
      div(input(tpe := "range", min := 0, max := maxv, step := steps, value := maxv, inputInt --> h)),
      div(span("+", child <-- h), s" $name"))
  }

  def renderEquippable(unit: Option[UnitData], ps: SkillEffect.CollatedEffect): List[VNode] = {
    def canEq(tpe: Int) = {
      val cls2 = if (ps.canEquip(tpe, unit)) "can" else "cant"
      span(cls := "icon-equips " + cls2 + "-equip")
    }
    val row1 = List(1,2,3,4,5,6,7,8,30,31,40,41)
    val row2 = List(9,10,11,12,13,14,15,16,50,51,52,53)
    List(
      div(row1.map(canEq):_*),
      div(row2.map(canEq):_*)
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
  def unitStats(unitInfo: Observable[Option[UnitData]], unit: Observable[Option[UnitEntry]], stats: Observable[Option[BaseStats]], equipped: Observable[(Equipped,Abilities)], allPassives: Observable[SkillEffect.CollatedEffect], esper: Observable[Option[EsperStatInfo]], esperEntry: Observable[Option[EsperEntry]]) = {
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

          (st.asStats * passives + e + eqstats + dhstats ++ ee, passives, pasv.dh, !is2h && isSW)
        }
    }
    table(cls := "unit-stats",
      caption("Effective Stats"),
      children <-- unit.combineLatest(unitInfo, allPassives, effective).combineLatest(equipped).map { case ((u,ui,pasv,eff),(eqs,abis)) =>
      def st(f: Stats => Int) = eff.fold("???")(d => f(d._1).toString)
        u.fold(List.empty[VNode]) { entry =>
          List(
            tr(td(List(colspan := 4) ++ renderEquippable(ui, pasv):_*)),
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
      List(
        tr(
            td(cls := "unit-stat-name", "HP"),
            td(cls := "unit-stat-data", st(_.hp)),
            td(cls := "unit-stat-name", "MP"),
            td(cls := "unit-stat-data", st(_.mp)),
          ),
          tr(
            td(cls := "unit-stat-name", "ATK"),
            td(cls := "unit-stat-data", st(_.atk)),
            td(cls := "unit-stat-name", "DEF"),
            td(cls := "unit-stat-data", st(_.defs))
          ),
          tr(
            td(cls := "unit-stat-name", "MAG"),
            td(cls := "unit-stat-data", st(_.mag)),
            td(cls := "unit-stat-name", "SPR"),
            td(cls := "unit-stat-data", st(_.spr))
          )) ++
            renderEquipped(ui, eqs.allEquipped, abis.allEquipped) ++
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

  def renderEquipped(u: Option[UnitData], eqs: List[EquipIndex], abis: List[MateriaIndex]) = {
    List(tr(td(colspan := 4, ul(
      (eqs.map(e => s"${e.name}: ${e.stats} ${e.describeEffects(u)}") ++
        abis.map(e => s"${e.name}: ${e.describeEffects(u)}")).map(n => li(n)):_*
    ))))
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
  def abilitySlots(m: Observable[List[MateriaIndex]], unitInfo: Observable[Option[UnitData]], up: Observable[Seq[SkillEffect]], unitEntry: Observable[Option[UnitEntry]], sorting: Observable[Sort], subject: AbilitySubjects): (MaybeMateria,MaybeMateria,MaybeMateria,MaybeMateria,Observable[List[VNode]]) = {
    val ability1Id = createIdHandler(None)
    val ability1 = materiaFor(m, ability1Id.merge(subject.a1)).publishReplay(1).refCount
    val ability2Id = createIdHandler(None)
    val ability2 = materiaFor(m, ability2Id.merge(subject.a2)).publishReplay(1).refCount
    val ability3Id = createIdHandler(None)
    val ability3 = materiaFor(m, ability3Id.merge(subject.a3)).publishReplay(1).refCount
    val ability4Id = createIdHandler(None)
    val ability4 = materiaFor(m, ability4Id.merge(subject.a4)).publishReplay(1).refCount
    (ability1, ability2, ability3, ability4) + 
    unitInfo.combineLatest(unitEntry).map { case (u, e) =>

      val slots = e.fold(0)(_.abilitySlots)

      def materiaList(w: MaybeMateria) = materiaOption(m, up, u, e, sorting, w)
      val m1s = materiaList(ability1)
      val m2s = materiaList(ability2)
      val m3s = materiaList(ability3)
      val m4s = materiaList(ability4)

      if (slots == 0) {
        subject.a1.next(None)
        subject.a2.next(None)
        subject.a3.next(None)
        subject.a4.next(None)
        Nil
      } else if (slots == 1) {
        subject.a2.next(None)
        subject.a3.next(None)
        subject.a4.next(None)
        List(tr(
          mslot("Ability 1", m1s, ability1Id)
        ))
      } else if (slots == 2) {
        subject.a3.next(None)
        subject.a4.next(None)
        List(tr(
          mslot("Ability 1", m1s, ability1Id),
          mslot("Ability 2", m2s, ability2Id)))
      } else if (slots == 3) {
        subject.a4.next(None)
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
