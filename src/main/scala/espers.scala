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
    checker: Observable[Map[Int,Boolean]]) = {
    val (nodes, sinks) = board.zipWithIndex.collect {
      case (EsperSlot(r@EsperSkillEffectReward(name, desc, effs), cost), i)
        if inRarity(rarity, cost) =>
        val checkSink = createHandler[(Int, EsperSkillEffectReward, Boolean)]((i, r, true))

        val node = span(label(
          input(tpe := "checkbox", inputChecked(b => (i, r, b)) --> checkSink,
            prop("checked") <-- checkCheck(i, checker, allCheck)
          ), "\u00a0", name.replaceAll(" ", "\u00a0")))
        node -> checkSink.merge(allCheck.map { b => (i,r,b) })
    }.unzip

    sink <-- Observable.combineLatest(sinks).map {
      _.foldLeft(Map.empty[Int,(EsperSkillEffectReward,Boolean)]) {
        case (ac, (i,r,b)) =>
        ac + ((i,(r,b)))
      }
    }
    nodes
  }

  // workaround for https://github.com/OutWatch/outwatch/commit/55687e9204f708ab7b1525d002143950ed0b826e#commitcomment-23957764
  def allAreChecked(checked: Observable[Map[Int,(_,Boolean)]]) =
    checked.map(d => if (d.values.forall(_._2)) "true" else "")
  def checkCheck(i: Int, src: Observable[Map[Int,Boolean]], all: Observable[Boolean]) =
    src.collect { case a if a.getOrElse(i, false) => a.getOrElse(i, false) }.merge(all).map(b => if (b) "true" else "")
  def trainEsperSkill(esperboard: Observable[List[EsperSlot]], esperRarity: Observable[Int], outs: Handler[Map[Int, (EsperSkillEffectReward,Boolean)]], checker: Observable[Map[Int,Boolean]]) = {
    esperboard.combineLatest(esperRarity) map { case (board,rarity) =>
      val allChecked = createBoolHandler(true)
      List(
        th(label(input(tpe := "checkbox", prop("checked") <-- allAreChecked(outs), inputChecked --> allChecked), "\u00a0", "Skills")),
        td(esperskills(board, rarity, outs, allChecked, checker):_*)
      )
    }
  }
  def esperstats(board: List[EsperSlot], rarity: Int,
    sink: Sink[Map[Int, (EsperStatReward, Boolean)]],
    allCheck: Observable[Boolean],
    checker: Observable[Map[Int,Boolean]], f: EsperStatReward => Option[Int]) = {
    val (nodes, sinks) = board.zipWithIndex.collect {
      case (EsperSlot(r@EsperStatReward(_,_,_,_,_,_), cost), i)
        if inRarity(rarity, cost) && f(r).nonEmpty =>
        val checkSink = createHandler[(Int, EsperStatReward, Boolean)]()

        val node = span(label(
          input(tpe := "checkbox", inputChecked(b => (i, r, b)) --> checkSink,
            prop("checked") <-- checkCheck(i, checker, allCheck)
          ), "\u00a0", f(r).get.toString))
        node -> checkSink.merge(allCheck.map { b => (i,r,b) })
    }.unzip

    sink <-- Observable.combineLatest(sinks).map {
      _.foldLeft(Map.empty[Int,(EsperStatReward,Boolean)]) {
        case (ac, (i,r,b)) =>
        ac + ((i,(r,b)))
      }
    }
    nodes
  }
  def trainEsperStat(stat: String, esperboard: Observable[List[EsperSlot]], esperRarity: Observable[Int], outs: Handler[Map[Int, (EsperStatReward,Boolean)]], f: EsperStatReward => Option[Int], checker: Observable[Map[Int,Boolean]]) = {
    esperboard.combineLatest(esperRarity) map { case (board,rarity) =>
      val allChecked = createBoolHandler(true)
      List(
        th(label(input(tpe := "checkbox", prop("checked") <-- allAreChecked(outs), inputChecked --> allChecked), "\u00a0", stat)),
        td(esperstats(board, rarity, outs, allChecked, checker, f):_*)
      )
    }
  }
  def esperInfo(esper: Handler[Option[EsperData]], esperEntry: Handler[Option[EsperEntry]], esperRaritySink: Handler[String], espers: Observable[Map[String,Int]], esperIdSubject: Subject[Option[String]], esperStats: Handler[Option[EsperStatInfo]], esperSkills: Handler[List[(String,List[String],List[SkillEffect])]], esperTraining: outwatch.Sink[Map[Int,Boolean]], trainingSubject: Subject[Map[Int,Boolean]]) = {
    val esperSink = createStringHandler()
    val esperId = esperSink.map(maybeId).merge(esperIdSubject).share
    esper <-- esperId.flatMap { e =>
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
                  else Observable.just(EsperSlot(EsperSkillEffectReward(s.name, s.effects, s.skilleffects), cost))
                }.catchError(_ => Observable.just(u))
              case x => Observable.just(x)
            }
          ).map(x => x.toList)
        }
      }
    }.share
    val esperRarity = esperRaritySink.map(r =>
      util.Try(r.toInt).toOption.getOrElse(1)
    )
    esperEntry <-- esper.combineLatest(esperRarity).map { case (e,r) =>
      e.map(_.entries(r))
    }

    def createStatSink()  =
      createHandler[Map[Int,(EsperStatReward,Boolean)]](Map.empty)
    val skillSink = createHandler[Map[Int,(EsperSkillEffectReward,Boolean)]](Map.empty)
    val hpSink  = createStatSink()
    val mpSink  = createStatSink()
    val atkSink = createStatSink()
    val defSink = createStatSink()
    val magSink = createStatSink()
    val sprSink = createStatSink()
    val allSink = Observable.combineLatest(List(hpSink, mpSink, atkSink, defSink, magSink, sprSink))
    esperTraining <-- skillSink.combineLatest(allSink).map { case (sks, sts) =>
      sks.map { case (k,v) => k -> v._2 } ++
        sts.foldLeft(Map.empty[Int,Boolean]) { (ac, m) =>
          ac ++ (m.map { case (k,v) => k -> v._2 })
        }
    }
    val selStats = allSink.map {
      _.foldLeft(List.empty[(Int,EsperStatReward)]) {
        case (ac, maps) =>
          ac ++ maps.toList.collect { case (k,v) if v._2 => (k,v._1) }
      }
    }
    val modifiedStats = esperEntry.combineLatest(selStats).map { case (e, sel) =>
      e.fold(Option.empty[EsperStatInfo]){ entry =>
        Some(sel.foldLeft(entry.stats) { (a,b) => a + b._2 })
      }
    }.share
    esperStats <-- modifiedStats
    esperSkills <-- skillSink.map { m =>
      m.values.toList.collect { case (esr, b) if b =>
        (esr.name, esr.desc, esr.effects)
      }
    }
    def esperStat(stat: String, f: EsperStatInfo => Int) =
      modifiedStats.map { 
        _.fold("")(x => f(x) + stat)
      }

    val trainClicks = createHandler[Unit](())
    val showTrainer = trainClicks.scan(false) { (show,_) => !show }
    val infoTable = div(id := "esper-container",
      select(children <-- espers.map { es =>
        val names = es.keys.toList.sorted
        option(value := EMPTY, "-- Select Esper --") ::
          names.map(n => option(value := es(n), n))
      }, inputString --> esperSink, value <-- espers.combineLatest(esperIdSubject).map(_._2).map(_.getOrElse(EMPTY)).startWith(EMPTY)),
      span(hidden <-- esper.map(_.isEmpty), "\u00a0",
        select(children <-- esper.map { e =>
          e.fold(List.empty[VNode])(_.entries.zipWithIndex.map { case (_, i) =>
            option(value := i, s"${i+1} \u2605", selected := i == 1)
          })
          },
          inputString --> esperRaritySink),
          "\u00a0",
        button(id := "esper-skills", tpe := "button", click(()) --> trainClicks, "Train Esper"),
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
        table(id := "esper-training", hidden <-- showTrainer,
          tr(children <-- trainEsperSkill(esperboard, esperRarity, skillSink, trainingSubject)),
          tr(children <-- trainEsperStat("HP",  esperboard, esperRarity, hpSink, _.maybeHP, trainingSubject)),
          tr(children <-- trainEsperStat("MP",  esperboard, esperRarity, mpSink, _.maybeMP, trainingSubject)),
          tr(children <-- trainEsperStat("ATK", esperboard, esperRarity, atkSink, _.maybeATK, trainingSubject)),
          tr(children <-- trainEsperStat("DEF", esperboard, esperRarity, defSink, _.maybeDEF, trainingSubject)),
          tr(children <-- trainEsperStat("MAG", esperboard, esperRarity, magSink, _.maybeMAG, trainingSubject)),
          tr(children <-- trainEsperStat("SPR", esperboard, esperRarity, sprSink, _.maybeSPR, trainingSubject)),
        )
      )
    )
    infoTable
  }
}
