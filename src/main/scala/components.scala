package yaffbedb

import outwatch.dom.{math => _,_}
import outwatch.Sink
import rxscalajs.Observable

object components {

  def sortBy(out: Sink[Sort]): VNode = {
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
        inputChecked(s) --> h, checked := check), n)

    div(cls := "sort-options", span("Sort"),
      sortItem(Sort.AZ,  sortAZ,  "A-Z", true),
      sortItem(Sort.HP,  sortHP,  "HP"),
      sortItem(Sort.MP,  sortMP,  "MP"),
      sortItem(Sort.ATK, sortATK, "ATK"),
      sortItem(Sort.DEF, sortDEF, "DEF"),
      sortItem(Sort.MAG, sortMAG, "MAG"),
      sortItem(Sort.SPR, sortSPR, "SPR"))
  }
  def unitBaseStats(unit: Observable[Option[UnitEntry]], stats: Sink[Option[BaseStats]], sub: PotSubjects) = {
    def potsFor(unit: Option[UnitEntry], f: StatInfo => StatRange): Int =
      unit.fold(0)(e => f(e.stats).pots)
    def statOf(unit: Option[UnitEntry],
      f: StatInfo => StatRange): Int =
        unit.fold(0)(e => f(e.stats).maxpots)
    def maxstat(unit: Option[UnitEntry],
      withPots: Observable[Int], f: StatInfo => StatRange) =
      withPots.map { p =>
        unit.fold(0)(e => f(e.stats).max + p).toString
      }

    // unit.map followed by stats <-- unit.combineLatest causes a
    // desync/bad subscription
    var subscription = Option.empty[rxscalajs.subscription.AnonymousSubscription]
    unit.filter(_.nonEmpty).map { u =>
      val potClicks = createHandler[Unit](())
      val showPots = potClicks.scan(false) { (show,_) => !show }
      def createIntHandler(x: Int) = createHandler[Int](x)
      val hpPots  = createIntHandler(potsFor(u, _.hp))
      val mpPots  = createIntHandler(potsFor(u, _.mp))
      val atkPots = createIntHandler(potsFor(u, _.atk))
      val defPots = createIntHandler(potsFor(u, _.defs))
      val magPots = createIntHandler(potsFor(u, _.mag))
      val sprPots = createIntHandler(potsFor(u, _.spr))
      subscription.foreach(_.unsubscribe())
      subscription = Some(stats <-- unit.combineLatest(hpPots.merge(sub.hp).combineLatest(mpPots.merge(sub.mp)).combineLatest(atkPots.merge(sub.atk).combineLatest(defPots.merge(sub.defs), magPots.merge(sub.mag), sprPots.merge(sub.spr)))).map {
        case (entry, ((hp,mp),(atk,defs,mag,spr))) =>
        for {
          e <- entry.orElse(u)
        } yield BaseStats(
          e.stats.hp.max + hp,
          e.stats.mp.max + mp,
          e.stats.atk.max + atk,
          e.stats.defs.max + defs,
          e.stats.mag.max + mag,
          e.stats.spr.max + spr,
          Pots(hp, mp, atk, defs, mag, spr)
        )
      }.filter(_.nonEmpty))
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

  def breakStat(stat: Observable[Int], break: Observable[Int]): Observable[Int] =
    stat.combineLatest(break).map { case (d,b) =>
      math.max(1, (d * (1.0 - b/100.0)).toInt)
    }
  def buffStat(stat: Observable[Int], buff: Observable[Int]): Observable[Int] =
    stat.combineLatest(buff).map { case (s, b) =>
      (s * (1 + b/100.0)).toInt
    }
  def getDefStat(stats: Observable[Option[UnitStats]], base: Observable[Option[BaseStats]], buff: Observable[Int], sel1: UnitStats => Int, sel2: BaseStats => Int): Observable[Option[Int]] = {
    stats.combineLatest(base, buff).map { case (s, b, bf) =>
      for {
        st <- s
        bs <- b
      } yield {
        sel1(st) + (sel2(bs) * (bf/100.0)).toInt
      }
    }
  }

  def calcKillers(target: TargetStats, killers: Map[Int,(Int,Int)], sel: ((Int,Int)) => Int): Int = {
    val ts = target.tribes.size
    val ks = target.tribes.map(t => sel(killers.getOrElse(t, (0,0)))).sum
    ks / ts
  }

  def calcElements(unit: Option[UnitStats], target: TargetStats): Int = {
    unit.fold(0) { u =>
      val es = u.elements
      val est = es.map(e =>
        target.resists.asMap.getOrElse(e, 0)).sum
      if (es.isEmpty) 0 else est / es.size
    }
  }
  case class Damage(min: Int, max: Int, avg: Int) {
    override def toString = s"$min - $max (avg $avg)"
    def +(o: Damage) = Damage(min + o.min, max + o.max, avg + o.avg)
  }
  object Damage {
    def empty = Damage(0, 0, 0)
  }

  case class DamageScoreSim(
    phy: Damage, dwL: Damage, dwR: Damage, dwT: Damage, mag: Damage)

  object DamageScoreSim {
    def empty = DamageScoreSim(
      Damage.empty, Damage.empty, Damage.empty, Damage.empty, Damage.empty)
  }

  def calculateDamageReceived(stat: Observable[Int], ratio: Observable[Int], defs: Observable[Option[Int]], level: Observable[Int], reduc1: Observable[Int], reduc2: Observable[Int], variance: WeaponVariance = WeaponVariance(1, 1)): Observable[Damage] = {
    stat.combineLatest(ratio, defs, level).combineLatest(reduc1, reduc2).map { case ((st, r, d, l), r1, r2) =>
    calculateDamageReceivedS(st, r, d, l, r1, r2, 0, 0)
    }
  }
  def calculateDamageReceivedS(st: Int, r: Int, d: Option[Int], l: Int, r1: Int, r2: Int, elements: Int, killers: Int, variance: WeaponVariance = WeaponVariance(1, 1)): Damage = {
      d.fold(Damage(0, 0, 0)) { df =>
        val dmg = ((1 + elements/100.0) * (1+ killers/100.0) * (1 - r1/100.0) * (1 - r2/100.0) * (1 + l/100.0) * (r/100.0) * ((st * st) / df)).toInt
        val min = (dmg * 0.85 * variance.min).toInt
        val max = (dmg * variance.max).toInt
        val avg = (min + max) / 2
        Damage(min, max, avg)
      }
  }
  def battleStats(baseStats: Observable[Option[BaseStats]], unitStats: Observable[Option[UnitStats]]): Observable[VNode] = Observable.just {
    val statsClicks = createHandler[Unit](())
    val showStats = statsClicks.scan(false) { (show,_) => !show }
    val atkBuff = createHandler[Int](0)
    val defBuff = createHandler[Int](0)
    val magBuff = createHandler[Int](0)
    val sprBuff = createHandler[Int](0)
    val sprBreak = createHandler[Int](0)
    val defBreak = createHandler[Int](0)
    val atkBreak = createHandler[Int](0)
    val magBreak = createHandler[Int](0)

    val targetSpr = createHandler[Int](25)
    val targetDef = createHandler[Int](25)
    val targetAtk = createHandler[Int](500)
    val targetMag = createHandler[Int](500)
    val targetLevel = createHandler[Int](99)

    val fireRes    = createHandler[Int](0)
    val iceRes     = createHandler[Int](0)
    val thunderRes = createHandler[Int](0)
    val waterRes   = createHandler[Int](0)
    val windRes    = createHandler[Int](0)
    val earthRes   = createHandler[Int](0)
    val holyRes    = createHandler[Int](0)
    val darkRes    = createHandler[Int](0)

    val damageRatio = createHandler[Int](1000)
    val outputRatio = createHandler[Int](100)
    val damageReduction = createHandler[Int](0)
    val physReduction = createHandler[Int](0)
    val magReduction = createHandler[Int](0)

    val tribeSink = createHandler[(Int,Boolean)]()
    val tribes = tribeSink.scan(Set(5)) { case (ac,(x,b)) =>
      if (b) ac + x else ac - x
    }.startWith(Set(5))

    val eleres = fireRes.combineLatest(iceRes, thunderRes, waterRes)
      .combineLatest(
        windRes.combineLatest(earthRes, holyRes, darkRes)).map {
          case ((fire, ice, thunder, water), (wind, earth, holy, dark)) =>
      ElementResist(fire, ice, thunder, water, wind, earth, holy, dark)
    }

    val targetStats = targetDef.combineLatest(targetSpr, defBreak, sprBreak)
      .combineLatest(eleres, tribes).map {
        case ((defs, spr, defB, sprB),eler, tribe) =>
      TargetStats(defs, spr, defB, sprB, tribe, eler)
    }

    val buffs = atkBuff.combineLatest(defBuff, magBuff, sprBuff).map {
      case (atk, defs, mag, spr) => Buffs(atk, defs, mag, spr)
    }

    val phyReceived  = calculateDamageReceived(breakStat(targetAtk, atkBreak),
      damageRatio,
      getDefStat(unitStats, baseStats, defBuff, _.defs, _.defs),
      targetLevel, damageReduction, physReduction)
    val magReceived = calculateDamageReceived(breakStat(targetMag, magBreak),
      damageRatio,
      getDefStat(unitStats, baseStats, sprBuff, _.spr, _.spr),
      targetLevel, damageReduction, magReduction)

    val dmgScore: Observable[DamageScoreSim] = {
      unitStats.combineLatest(
        baseStats, targetStats).combineLatest(
        defBreak.combineLatest(sprBreak), atkBuff.combineLatest(magBuff), outputRatio).map { case ((u,b,t),(db,sb),(ab,mb),r) =>
        (for {
          s <- u
          a <- b
        } yield {
          val dwadj = if (s.l != 0 && s.l != 0) s.l
          else 0
          println("variance: " + s.variance)
          val phy = calculateDamageReceivedS(
            (s.atk + a.atk * (1 + ab/100.0)).toInt - dwadj,
            r,
            Option((t.defs * (1 - db/100.0)).toInt),
            s.level, 0, 0,
            calcKillers(t, u.fold(Map.empty[Int,(Int,Int)])(_.killers), _._1),
            calcElements(u, t),
            s.variance)
          val (dw1, dw2) = if (s.dw) {
            val dwR = calculateDamageReceivedS(
              (s.atk + a.atk * (1 + ab/100.0)).toInt - s.l,
              r,
              Option((t.defs * (1 - db/100.0)).toInt),
              s.level, 0, 0,
              calcKillers(t, u.fold(Map.empty[Int,(Int,Int)])(_.killers), _._1),
              calcElements(u, t),
              s.variance)
            val dwL = calculateDamageReceivedS(
              (s.atk + a.atk * (1 + ab/100.0)).toInt - s.r,
              r,
              Option((t.defs * (1 - db/100.0)).toInt),
              s.level, 0, 0,
              calcKillers(t, u.fold(Map.empty[Int,(Int,Int)])(_.killers), _._1),
              calcElements(u, t),
              s.variance)
              List(
                div("Physical DW R: " + dwR),
                div("Physical DW L: " + dwL)
              )
            dwR -> dwL
          } else {
            Damage.empty -> Damage.empty
          }
          val mag = calculateDamageReceivedS(
            (s.mag + a.mag * (1 + mb/100.0)).toInt,
            r,
            Option((t.spr * (1 - sb/100.0)).toInt),
            s.level, 0, 0,
            calcKillers(t, u.fold(Map.empty[Int,(Int,Int)])(_.killers), _._2),
            0, s.variance)
          DamageScoreSim(phy, dw1, dw2, dw1 + dw2, mag)
        }).getOrElse(DamageScoreSim.empty)
      }
    }

    div(cls := "battle-stats",
      div(
        div("Buffs"),
        div("ATK ", child <-- atkBuff.map(_ + "%")),
        div("DEF ", child <-- defBuff.map(_ + "%")),
        div("MAG ", child <-- magBuff.map(_ + "%")),
        div("SPR ", child <-- sprBuff.map(_ + "%")),
        div(),
        button("Adjust", tpe := "button", click(()) --> statsClicks),
      ),
      div(
        div("Opponent"),
        div("ATK ", child <-- breakStat(targetAtk, atkBreak)),
        div("DEF ", child <-- breakStat(targetDef, defBreak)),
        div("MAG ", child <-- breakStat(targetMag, magBreak)),
        div("SPR ", child <-- breakStat(targetSpr, sprBreak)),
      ),
      div(cls := "dmg-score",
        div("Damage:"),
        div(child <-- dmgScore.map { d =>
          math.max(d.mag.avg, math.max(d.phy.avg, d.dwT.avg))
        }),
        div("Tanking:"),
        div(child <-- phyReceived.map(_.avg)),
        div(child <-- magReceived.map(_.avg))
      ),
      div(hidden <-- showStats, cls := "battle-stats-inner",
      h4("Unit Buffs"),
      div(
        numberPicker("% ATK", atkBuff, init = 0, min = 0),
        numberPicker("% DEF", defBuff, init = 0, min = 0),
        numberPicker("% MAG", magBuff, init = 0, min = 0),
        numberPicker("% SPR", sprBuff, init = 0, min = 0),
      ),
      h4("Opponent"),
      h5("Stats"),
      div(
        numberPicker("ATK", targetAtk, init = 500, min = 1),
        numberPicker("DEF", targetDef, init = 25, min = 1),
        numberPicker("MAG", targetMag, init = 500, min = 1),
        numberPicker("SPR", targetSpr, init = 25, min = 1),
        numberPicker("Level", targetLevel, init = 99, min = 1, max = 100),
      ),
      h5("Breaks"),
      div(
        numberPicker("% ATK", atkBreak, init = 0, min = 0, max = 99),
        numberPicker("% DEF", defBreak, init = 0, min = 0, max = 99),
        numberPicker("% MAG", magBreak, init = 0, min = 0, max = 99),
        numberPicker("% SPR", sprBreak, init = 0, min = 0, max = 99),
      ),
      h5("Damage Received"),
      div(
        numberPicker("Ratio", damageRatio, init = 1000, min = 1, max = 50000, steps = (10, 100, 500), n = r => f"${r / 100.0}%.1f", from = x => (x * 100).toInt),
        numberPicker("% Damage Reduction", damageReduction, init = 0, min = 0, max = 99),
        numberPicker("% Physical Reduction", physReduction, init = 0, min = 0, max = 99),
        numberPicker("% Magical Reduction", magReduction, init = 0, min = 0, max = 99),
        div("Physical: ", child <-- phyReceived),
        div("Magical: ", child <-- magReceived),
      ),
      /*
      h5("Elemental Resists"),
      div(
        numberPicker(span(cls := "elements fire"), fireRes, init = 0, min = -200, max = 200),
        numberPicker(span(cls := "elements ice"), iceRes, init = 0, min = -200, max = 200),
        numberPicker(span(cls := "elements thunder"), iceRes, init = 0, min = -200, max = 200),
        numberPicker(span(cls := "elements water"), thunderRes, init = 0, min = -200, max = 200),
        numberPicker(span(cls := "elements wind"), windRes, init = 0, min = -200, max = 200),
        numberPicker(span(cls := "elements earth"), earthRes, init = 0, min = -200, max = 200),
        numberPicker(span(cls := "elements holy"), holyRes, init = 0, min = -200, max = 200),
        numberPicker(span(cls := "elements dark"), darkRes, init = 0, min = -200, max = 200),
      ),
      */
      h5("Tribes"),
      div((cls := "target-tribes") ::
        SkillEffect.TRIBE.toList.sortBy(_._1).map { case (k,v) =>
          span(label(input(tpe := "checkbox",
            value := k, checked := k == 5, inputChecked(b => (k,b)) --> tribeSink), " ", v), " ")
        }: _*
      ),
      h5("Damage Score"),
      div(
        numberPicker("Ratio", outputRatio, init = 100, min = 1, max = 3000, steps = (10, 100, 500), n = r => f"${r / 100.0}%.1f", from = x => (x * 100).toInt),
        div(children <-- dmgScore.map { ds =>
          val dw = if (ds.dwT != Damage.empty) {
            List(
              div("Physical DW: " + ds.dwT),
              div("Physical DW R: " + ds.dwR),
              div("Physical DW L: " + ds.dwL),
            )
          } else Nil
          List(div("Physical SW: " + ds.phy)) ++
          dw ++ List(div("Magical: " + ds.mag))
        }),
      ),

    )
    )
  }

  def numberPicker(lbl: VNode, sink: Sink[Int],
    init: Int = 25, min: Int = -300, max: Int = 3000,
    steps: (Int, Int, Int) = (1, 5, 50),
    n: Int => String = _.toString,
    from: Double => Int = _.toInt): VNode = {
    val h = createHandler[Int]()
    val nh = createHandler[Int](init)
    val validator = rxscalajs.Subject[Int]()
    val result = withStamp(h.startWith(0)).combineLatest(withStamp(nh)).scan(init) {
      case (ac,((x,ts1),(y,ts2))) =>
      val z = if (ts1 > ts2) ac + x else y
      val r = math.min(max, math.max(min, z))
      if (z != r)
        validator.next(r)
      r
    }


    sink <-- result.merge(nh)

    div(cls := "number-picker",
      button("\u2193", click(-1 * steps._1) --> h),
      button("\u21e9", click(-1 * steps._2) --> h),
      button("\u2b07", click(-1 * steps._3) --> h),
      input(tpe := "number", inputNumber(from) --> nh, value <-- result.merge(validator).startWith(init).map(n)),
      button("\u2b06", click(steps._3) --> h),
      button("\u21e7", click(steps._2) --> h),
      button("\u2191", click(steps._1) --> h),
      " ",
      lbl,
    )
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
      tr(resists.map { r => td(s"$r%") }:_*)
    )
  }
  case class Effective(base: BaseStats, stats: Stats,
    passives: PassiveStatEffect,
    dh: PassiveDoublehandEffect, dhGE: PassiveSinglehandEffect,
    tdh: Passive2HEffect, tdhGE: PassiveTDHEffect, accuracy: Int,
    is1h: Boolean, is2h: Boolean,
    ed: Option[EsperData], e: Option[EsperStatInfo], ee: Option[EsperEntry],
    variance: WeaponVariance, atkL: Int, atkR: Int, killers: Map[Int,(Int,Int)])

  def unitStats(unitInfo: Observable[Option[UnitData]],
                unit: Observable[Option[UnitEntry]],
                stats: Observable[Option[BaseStats]],
                equipped: Observable[(Equipped,Abilities)],
                allPassives: Observable[SkillEffect.CollatedEffect],
                esperD: Observable[Option[EsperData]],
                esper: Observable[Option[EsperStatInfo]],
                esperEntry: Observable[Option[EsperEntry]],
                enhs: Observable[Map[Int,SkillInfo]],
                enhm: Observable[Map[Int,Int]],
                unitOut: Sink[Option[UnitStats]]) = {
    val effective = stats.combineLatest(esper.combineLatest(esperD, esperEntry), equipped, allPassives).map {
      case (s,(e,ed,ee),(eqs,_),pasv) =>
        s.map { st =>
          val alleq = eqs.allEquipped
          val atks = alleq.collect {
            case equip if equip.slotId == 1 => equip.stats.atk
          }
          val (l,r) = atks match {
            case Nil           => (0, 0)
            case x :: y :: Nil => (y, x)
            case x :: Nil      => (0, x)
            case xs            => (0, 0) // more than 2 weapons???
          }
          val variance = alleq.map(_.variance).find(
            _ != WeaponVariance.none).getOrElse(WeaponVariance(1,1))
          val passives = pasv.stats + pasv.statFromEquips(alleq)

          val is2h = alleq.exists(_.twohands)
          val isSW = alleq.count(_.slotId == 1) == 1 &&
            alleq.count(_.slotId == 2) == 0
          val eqstats = alleq.foldLeft(Stats.zero) { (ac, equip) =>
            ac + equip.stats
          }
          val eqaccy = alleq.map(_.accuracy).sum
          val dh = if (!is2h && isSW) pasv.dh.asSingleHand
          else PassiveSinglehandEffect.zero

          val dhGE = if (!is2h && isSW) pasv.dhGE
          else PassiveSinglehandEffect.zero

          val tdh = if (is2h || isSW) pasv.tdh.asSingleHand
          else PassiveSinglehandEffect.zero

          val tdhGE = if (is2h || isSW) pasv.tdhGE.asSingleHand
          else PassiveSinglehandEffect.zero

          val alldhGE = dhGE + tdhGE
          val alldh = dh + tdh

          val accuracy = (if (isSW) eqaccy else 0) + (if (is2h || isSW) pasv.tdh.accuracy else 0) + (if (!is2h && isSW) pasv.accuracy1h else 0)

          Effective(st, st.asStats * passives + e + eqstats + (eqstats * alldh) + (eqstats * alldhGE) ++ ee, passives, pasv.dh, pasv.dhGE, pasv.tdh, pasv.tdhGE, accuracy, !is2h && isSW, isSW || is2h, ed, e, ee, variance, l, r, pasv.killers)
        }
    }

    unitOut <-- effective.map { _.map(eff =>
      UnitStats(eff.stats.atk, eff.stats.defs, eff.stats.mag, eff.stats.spr, eff.atkL, eff.atkR, eff.variance, 100, Set.empty, eff.killers)
    )}
    table(cls := "unit-stats",
      caption("Effective Stats"),
      children <-- unit.combineLatest(unitInfo, allPassives, effective).combineLatest(equipped).combineLatest(enhs, enhm).map { case (((u,ui,pasv,eff),(eqs,abis)),es,em) =>
      def st(f: Stats => Int) = eff.fold("???")(d => f(d.stats).toString)
        u.fold(List.empty[VNode]) { entry =>
          List(
            tr(td(List(colspan := 4) ++ renderEquippable(ui, pasv):_*)),
            tr(td(colspan := 4,
              renderResists(
                (entry.statusResist + eff.fold(AilmentResist.zero)(_.stats.status) + pasv.statusResists.asAilmentResist).asList.map(_._1),
                "status-table")
            )),
            tr(td(colspan := 4,
              renderResists(
                (entry.elementResist + eff.fold(ElementResist.zero)(_.stats.element) + pasv.elementResists.asElementResist).asList.map(_._1),
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
            renderPots(eff) ++
            renderEquipped(ui, eqs.allEquipped, abis.allEquipped) ++
            renderEsper(eff) ++
            renderEnhancements(es, em) ++
            renderStat(statOf(eff, _.hp), "+HP") ++
            renderStat(statOf(eff, _.mp), "+MP") ++
            renderStat(statOf(eff, _.atk), "+ATK") ++
            renderStat(statOf(eff, _.defs), "+DEF") ++
            renderStat(statOf(eff, _.mag), "+MAG") ++
            renderStat(statOf(eff, _.spr), "+SPR") ++
            renderStat(dhOf(eff) + tdhOf(eff), "+Equip ATK") ++
            renderStat(dhGEOf(eff, _.hp) + tdhGEOf(eff, _.hp), "+GE Equip HP") ++
            renderStat(dhGEOf(eff, _.mp) + tdhGEOf(eff, _.mp), "+GE Equip MP") ++
            renderStat(dhGEOf(eff, _.atk) + tdhGEOf(eff, _.atk), "+GE Equip ATK") ++
            renderStat(dhGEOf(eff, _.defs) + tdhGEOf(eff, _.defs), "+GE Equip DEF") ++
            renderStat(dhGEOf(eff, _.mag) + tdhGEOf(eff, _.mag), "+GE Equip MAG") ++
            renderStat(dhGEOf(eff, _.spr) + tdhGEOf(eff, _.spr), "+GE Equip SPR") ++
            renderStat(eff.fold(0)(_.accuracy), "Accuracy", max = 100) ++
            renderStat(statOf(eff, _.crit) + 10, "Crit chance", max = 100) ++
            renderDodge(pasv.dodge) ++
            renderKillers(pasv.killers) ++
            renderStat(pasv.lbrate, "+LB fill") ++
            renderStat(pasv.lbfill / 100, "LB/turn", pct = false, max = 12) ++
            renderStat(pasv.jump, "+Jump Damage") ++
            renderStat(pasv.evomag, "+EVO MAG") ++
            renderStat((eff.fold(0)(_.stats.mp) * (pasv.refresh / 100.0)).toInt,
              pasv.refresh + "% MP/turn", pct = false) ++
            renderStat(pasv.attract, "Draw Attacks") ++
            renderStat(pasv.camouflage, "Camouflage")
        }
      }
    )
  }

  def renderEquipped(u: Option[UnitData], eqs: List[EquipIndex], abis: List[MateriaIndex]) = {
    List(tr(td(colspan := 4, div(
      (eqs.map(e => s"${e.name}: ${e.stats} ${e.describeEffects(u)}") ++
        abis.map(e => s"${e.name}: ${e.describeEffects(u)}")).map(n => div(n)):_*
    ))))

  }
  def renderPots(eff: Option[Effective]): List[VNode] = {
    def st(f: Pots => Int) = eff.fold("???")(d => f(d.base.pots).toString)
    List(
      tr(
        td(colspan := 4, div(s"Pots: ${st(_.hp)}HP ${st(_.mp)}MP ${st(_.atk)}ATK ${st(_.defs)}DEF ${st(_.mag)}MAG ${st(_.spr)}SPR"))
      )
    )
  }
  def renderEnhancements(enhs: Map[Int,SkillInfo], enhm: Map[Int,Int]): List[VNode] = {
    val es = enhm.toList.foldLeft(List.empty[String]) { case (ac, (x, y)) =>
      enhancementsOf(x, enhs).fold(ac) { case (p1, p2) =>
        if (p1.id == y) {
          (p1.name + " +1") :: ac
        } else if (p2.id == y) {
          (p2.name + " +2") :: ac
        } else {
          ac
        }
      }
    }

    if (es.isEmpty) Nil
    else List(tr(td(colspan := 4, div(es.map(div(_)):_*))))
  }

  def statOf(x: Option[Effective], f: PassiveStatEffect => Int): Int = x.fold(0)(d => f(d.passives))
  def dhGEOf(x: Option[Effective], f: PassiveSinglehandEffect => Int): Int = x.fold(0)(d => if (d.is1h) f(d.dhGE) else 0)
  def tdhGEOf(x: Option[Effective], f: PassiveTDHEffect => Int): Int = x.fold(0)(d => if (d.is2h) f(d.tdhGE) else 0)
  def dhOf(x: Option[Effective]): Int = x.fold(0)(d => if (d.is1h) d.dh.dh else 0)
  def tdhOf(x: Option[Effective]): Int = x.fold(0)(d => if (d.is2h) d.tdh.dh else 0)

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
  def renderEsper(eff: Option[Effective]): List[VNode] = {
    (for {
      ef <- eff
      d <- ef.ed
      s <- ef.e
      e <- ef.ee
    } yield {
      val idx = d.entries.indexOf(e)
      List(
        tr(
          td(colspan := 4, div("Esper: " + d.names.headOption.getOrElse("") + " " + (idx+1) + "\u2605": String),
            div(cls := "esper-stats", s"${s.hp.max}HP ${s.mp.max}MP ${s.atk.max}ATK ${s.defs.max}DEF ${s.mag.max}MAG ${s.spr.max}SPR")
          )
        ),
      )
    }).getOrElse(Nil)
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

    table((cls := tableCls) :: rows: _*)
  }

  def effectiveStats(u: UnitData, equip: MateriaIndex, pasv: SkillEffect.CollatedEffect): PassiveStatEffect = {
    val innates = SkillEffect.collateEffects(Some(u), equip.skillInfo.flatMap(_.passives))

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
  def materiaOption(m: List[MateriaIndex], up: Observable[Seq[SkillEffect]], u: Option[UnitData], e: Option[UnitEntry], sorting: Observable[Sort], worn: Observable[Option[MateriaIndex]]): Observable[List[VNode]] = up.combineLatest(sorting, worn).map {
    case (ps, s, w) =>
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
  def abilitySlots(m: Observable[List[MateriaIndex]], unitInfo: Observable[Option[UnitData]], up: Observable[Seq[SkillEffect]], unitEntry: Observable[Option[UnitEntry]], sorting: Observable[Sort], subject: AbilitySubjects, validators: AbilitySubjects): (MaybeMateria,MaybeMateria,MaybeMateria,MaybeMateria,Observable[List[VNode]]) = {
    val ability1Id = createIdHandler(None)
    val ability1 = materiaFor(m, ability1Id.merge(subject.a1).distinctUntilChanged).publishReplay(1).refCount
    val ability2Id = createIdHandler(None)
    val ability2 = materiaFor(m, ability2Id.merge(subject.a2).distinctUntilChanged).publishReplay(1).refCount
    val ability3Id = createIdHandler(None)
    val ability3 = materiaFor(m, ability3Id.merge(subject.a3).distinctUntilChanged).publishReplay(1).refCount
    val ability4Id = createIdHandler(None)
    val ability4 = materiaFor(m, ability4Id.merge(subject.a4).distinctUntilChanged).publishReplay(1).refCount
    (ability1, ability2, ability3, ability4) + 
    unitInfo.combineLatest(unitEntry,m).map { case (u, e, ms) =>

      val slots = e.fold(-1)(_.abilitySlots)

      def materiaList(w: MaybeMateria) = materiaOption(ms, up, u, e, sorting, w)
      val m1s = materiaList(ability1)
      val m2s = materiaList(ability2)
      val m3s = materiaList(ability3)
      val m4s = materiaList(ability4)

      if (slots == -1) {
        Nil
      } else if (slots == 0) {
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
          mslot("Ability 1", m1s, ability1Id, validators.a1)
        ))
      } else if (slots == 2) {
        subject.a3.next(None)
        subject.a4.next(None)
        List(tr(
          mslot("Ability 1", m1s, ability1Id, validators.a1),
          mslot("Ability 2", m2s, ability2Id, validators.a2)))
      } else if (slots == 3) {
        subject.a4.next(None)
        List(
          tr(
            mslot("Ability 1", m1s, ability1Id, validators.a1),
            mslot("Ability 2", m2s, ability2Id, validators.a2)),
          tr(mslot("Ability 3", m3s, ability3Id, validators.a3)))
      } else {
        List(
          tr(
            mslot("Ability 1", m1s, ability1Id, validators.a1),
            mslot("Ability 2", m2s, ability2Id, validators.a2)),
          tr(
            mslot("Ability 3", m3s, ability3Id, validators.a3),
            mslot("Ability 4", m4s, ability4Id, validators.a4)))
      }
    }
  }

  def mslot(name: String, cs: Observable[List[VNode]], sink: Sink[Option[String]], subject: rxscalajs.Subject[Option[String]]): VNode =
    td(label(name, select(cls := "equip-slot", children <-- cs, inputId --> sink, value <-- subject.map(_.getOrElse(EMPTY)).startWith(EMPTY))))

}
