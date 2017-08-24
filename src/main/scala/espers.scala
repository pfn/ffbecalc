package yaffbedb

import rxscalajs.Observable
import rxscalajs.Subject
import boopickle.Default._
import outwatch.dom._
import outwatch.Sink

object Esper {
  def inRarity(rarity: Int, cost: Int): Boolean = {
    if (cost > 50) false
    if (cost >= 40 && rarity == 2) true
    else if (cost <= 30 && rarity >= 1) true
    else if (cost < 30) true
    else false
  }
  def esperskills(board: List[EsperSlot], rarity: Int,
    sink: Sink[Map[Int, (EsperSkillEffectReward, Boolean)]],
    allCheck: Observable[Boolean],
    checker: Observable[(Int,Boolean)]) = {
    val (nodes, sinks) = board.zipWithIndex.collect {
      case (EsperSlot(r@EsperSkillEffectReward(name, effs), cost), i)
        if inRarity(rarity, cost) =>
        val checkSink = createHandler[(Int, EsperSkillEffectReward, Boolean)]()

        val node = span(label(
          input(tpe := "checkbox", inputChecked(b => (i, r, b)) --> checkSink,
            checked <-- checker.collect {
              case (a,b) if a == i => b
            }.merge(allCheck).startWith(true)
          ), "\u00a0", name.replaceAll(" ", "\u00a0")))
        node -> checkSink.startWith((i,r,true))
    }.unzip

    sink <-- Observable.combineLatest(sinks).map {
      _.foldLeft(Map.empty[Int,(EsperSkillEffectReward,Boolean)]) {
        case (ac, (i,r,b)) =>
        ac + ((i,(r,b)))
      }
    }
    nodes
  }
  def trainEsperSkill(esperboard: Observable[List[EsperSlot]], esperRarity: Observable[Int]) = {
    esperboard.combineLatest(esperRarity) map { case (board,rarity) =>
      val checker = Subject[(Int,Boolean)]()
      val outs = createHandler[Map[Int, (EsperSkillEffectReward,Boolean)]]()
      val allChecked = createBoolHandler()
      List(
        th(label(input(tpe := "checkbox", checked <-- outs.map(_.values.forall(_._2)).startWith(true), inputChecked --> allChecked), "\u00a0", "Skills")),
        td(esperskills(board, rarity, outs, allChecked, checker):_*)
      )
    }
  }
  def esperstats(board: List[EsperSlot], rarity: Int,
    sink: Sink[Map[Int, (EsperStatReward, Boolean)]],
    allCheck: Observable[Boolean],
    checker: Observable[(Int,Boolean)], f: EsperStatReward => Option[Int]) = {
    val (nodes, sinks) = board.zipWithIndex.collect {
      case (EsperSlot(r@EsperStatReward(_,_,_,_,_,_), cost), i)
        if inRarity(rarity, cost) && f(r).nonEmpty =>
        val checkSink = createHandler[(Int, EsperStatReward, Boolean)]()

        val node = span(label(
          input(tpe := "checkbox", inputChecked(b => (i, r, b)) --> checkSink,
            checked <-- checker.collect {
              case (a,b) if a == i => b
            }.merge(allCheck).startWith(true)
          ), "\u00a0", f(r).get.toString))
        node -> checkSink.merge(allCheck.map { b => (i,r,b) }).startWith((i,r,true))
    }.unzip

    sink <-- Observable.combineLatest(sinks).map {
      _.foldLeft(Map.empty[Int,(EsperStatReward,Boolean)]) {
        case (ac, (i,r,b)) =>
        ac + ((i,(r,b)))
      }
    }
    nodes
  }
  def trainEsperStat(stat: String, esperboard: Observable[List[EsperSlot]], esperRarity: Observable[Int], outs: Handler[Map[Int, (EsperStatReward,Boolean)]], f: EsperStatReward => Option[Int]) = {
    esperboard.combineLatest(esperRarity) map { case (board,rarity) =>
      val checker = Subject[(Int,Boolean)]()
      val allChecked = createBoolHandler()
      List(
        th(label(input(tpe := "checkbox", checked <-- outs.map(_.values.forall(_._2)).startWith(true), inputChecked --> allChecked), "\u00a0", stat)),
        td(esperstats(board, rarity, outs, allChecked, checker, f):_*)
      )
    }
  }
  def esperInfo(espers: Observable[Map[String,Int]], esperIdSubject: Subject[Option[String]], esperStats: Handler[Option[EsperStatInfo]]): (Observable[Option[EsperData]], VNode) = {
    val esperSink = createStringHandler()
    val esperId = esperSink.map(maybeId).merge(esperIdSubject)
    val esper: Observable[Option[EsperData]] = esperId.flatMap { e =>
      e.fold(Observable.just(Option.empty[EsperData])) { eid =>
        Data.get[EsperData](s"pickle/esper/${eid}.pickle").map(Some.apply)
      }
    }
    val esperboard: Observable[List[EsperSlot]] = esperId.flatMap { e =>
      e.fold(Observable.just(List.empty[EsperSlot])) { eid =>
        Data.get[List[EsperSlot]](s"pickle/esperboard/${eid}.pickle").flatMap { ss =>
          Observable.combineLatest(
            ss.map {
              case u@EsperSlot(EsperAbilityReward(id), cost) =>
                Data.get[SkillInfo](s"pickle/skill/$id.pickle").flatMap { s =>
                  if (s.active) Observable.just(EsperSlot(UnknownEsperSkill, 100))
                  else Observable.just(EsperSlot(EsperSkillEffectReward(s.name, s.skilleffects), cost))
                }.catchError(_ => Observable.just(u))
              case x => Observable.just(x)
            }
          ).map(x => x.toList)
        }
      }
    }
    val esperRaritySink = createStringHandler()
    val esperRarity = esperRaritySink.startWith("1").map(r =>
      util.Try(r.toInt).toOption.getOrElse(1)
    )
    val esperEntry = esper.combineLatest(esperRarity).map { case (e,r) =>
      e.map(_.entries(r))
    }

    val hpSink  = createHandler[Map[Int,(EsperStatReward,Boolean)]]()
    val mpSink  = createHandler[Map[Int,(EsperStatReward,Boolean)]]()
    val atkSink = createHandler[Map[Int,(EsperStatReward,Boolean)]]()
    val defSink = createHandler[Map[Int,(EsperStatReward,Boolean)]]()
    val magSink = createHandler[Map[Int,(EsperStatReward,Boolean)]]()
    val sprSink = createHandler[Map[Int,(EsperStatReward,Boolean)]]()
    val selStats = Observable.combineLatest(List(hpSink, mpSink, atkSink, defSink, magSink, sprSink)).map {
      _.foldLeft(List.empty[(Int,EsperStatReward)]) {
        case (ac, maps) =>
          ac ++ maps.toList.collect { case (k,v) if v._2 => (k,v._1) }
      }
    }.startWith(Nil)
    val modifiedStats = esperEntry.combineLatest(selStats).map { case (e, sel) =>
      e.fold(Option.empty[EsperStatInfo]){ entry =>
        Some(sel.foldLeft(entry.stats) { (a,b) => a + b._2 })
      }
    }
    esperStats <-- modifiedStats
    def esperStat(stat: String, f: EsperStatInfo => Int) =
      modifiedStats.map { 
        _.fold("")(x => f(x) + stat)
      }

    val infoTable = div(id := "esper-container",
      select(children <-- espers.map { es =>
        val names = es.keys.toList.sorted
        option(value := EMPTY, "-- Select Esper --") ::
          names.map(n => option(value := es(n), n))
      }, inputString --> esperSink, value <-- espers.combineLatest(esperIdSubject).map(_._2).map(_.getOrElse(EMPTY)).startWith(EMPTY)),
      span(hidden <-- esper.startWith(None).map(_.isEmpty), "\u00a0",
        select(children <-- esper.map { e =>
          e.fold(List.empty[VNode])(_.entries.zipWithIndex.map { case (_, i) =>
            option(value := i, s"${i+1} \u2605", selected := i == 1)
          })
          },
          inputString --> esperRaritySink),
          "\u00a0",
        button(id := "esper-skills", tpe := "button", "Train Esper"),
        div(id := "esper-stats",
          span(child <-- esperStat("HP", _.hp.max)),
          "\u00a0",
          span(child <-- esperStat("MP", _.mp.max)),
          "\u00a0",
          span(child <-- esperStat("ATK", _.atk.max)),
          "\u00a0",
          span(child <-- esperStat("DEF", _.defs.max)),
          "\u00a0",
          span(child <-- esperStat("MAG", _.mag.max)),
          "\u00a0",
          span(child <-- esperStat("SPR", _.spr.max))
        ),
        table(id := "esper-training",
          tr(children <-- trainEsperSkill(esperboard, esperRarity)),
          tr(children <-- trainEsperStat("HP",  esperboard, esperRarity, hpSink, _.maybeHP)),
          tr(children <-- trainEsperStat("MP",  esperboard, esperRarity, mpSink, _.maybeMP)),
          tr(children <-- trainEsperStat("ATK", esperboard, esperRarity, atkSink, _.maybeATK)),
          tr(children <-- trainEsperStat("DEF", esperboard, esperRarity, defSink, _.maybeDEF)),
          tr(children <-- trainEsperStat("MAG", esperboard, esperRarity, magSink, _.maybeMAG)),
          tr(children <-- trainEsperStat("SPR", esperboard, esperRarity, sprSink, _.maybeSPR)),
        )
      )
    )
    (esper, infoTable)
  }
}
