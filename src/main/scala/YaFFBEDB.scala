package yaffbedb

import scala.scalajs.js.JSApp
import org.scalajs.dom.document
import org.scalajs.dom.window

import outwatch.dom._
import rxscalajs.{Observable,Subject}
import boopickle.Default._
import scala.concurrent.duration.{span => _, _}

object YaFFBEDB extends JSApp {
  case class PageState(
    unit:  Option[Int],
    rhand: Option[Int],
    lhand: Option[Int],
    head:  Option[Int],
    body:  Option[Int],
    acc1:  Option[Int],
    acc2:  Option[Int],
    mat1:  Option[Int],
    mat2:  Option[Int],
    mat3:  Option[Int],
    mat4:  Option[Int],
    pots:  Pots,
    esper: Option[Int],
    esperRarity: Int,
    esperSkills: Map[Int,Boolean]
  )
  object PageState {
    def idOfEq(e: (Option[EquipIndex],Double)) = e._1.map(_.id)
    def idOfMat(e: (Option[MateriaIndex],Double)) = e._1.map(_.id)
    def from(unitId: Option[Int], stats: Option[BaseStats],
      eqs: Equipped, abis: Abilities,
      esper: Option[Int], esperR: Int,
      esperSkills: Map[Int,Boolean]) = PageState(
      unitId,
      idOfEq(eqs.rhand),      idOfEq(eqs.lhand),
      idOfEq(eqs.head),       idOfEq(eqs.body),
      idOfEq(eqs.acc1),       idOfEq(eqs.acc2),
      idOfMat(abis.ability1), idOfMat(abis.ability2),
      idOfMat(abis.ability3), idOfMat(abis.ability4),
      stats.fold(Pots.all)(_.pots),
      esper, esperR,
      esperSkills)
  }
  def main(): Unit = {
    val unitIdSubject = Subject[Option[String]]()
    val unitIdSink = createIdHandler(None)
    val unitId = unitIdSubject.merge(unitIdSink).distinct.publishReplay(1).refCount

    val unitIndex = Data.get[List[UnitIndex]]("pickle/unit/index.pickle").combineLatest(unitId.startWith(None)).map { case (us, id) =>
      List(option(value := EMPTY, "-- Select a unit --")) ++
        us.map { u =>
          val rarity = ("\u2605" * u.min) + ("\u2606" * (u.max - u.min))
          option(value := u.id, selected := id.exists(_ == u.id), s"${u.name}: $rarity")
        }
    }.publishReplay(1).refCount
    val pots = PotSubjects()

    val equips  = Data.get[List[EquipIndex]]("pickle/equip/index.pickle")
    val materia = Data.get[List[MateriaIndex]]("pickle/materia/index.pickle")
    val espers  = Data.get[Map[String,Int]]("pickle/esper/index.pickle")

    val unitInfo: Observable[Option[UnitData]] = unitId.flatMap(id => 
      id.fold(Observable.just(Option.empty[UnitData])) { id_ =>
        Data.get[UnitData](s"pickle/unit/$id_.pickle").map { u =>
          Some(u)
        }
      }).publishReplay(1).refCount
    val enhancements: Observable[Map[String,Enhancement]] = unitId.flatMap(id =>
      id.fold(Observable.just(Map.empty[String,Enhancement])) { id_ =>
        Data.get[Map[String,Enhancement]](s"pickle/enhance/$id_.pickle").catchError(_ => Observable.just(Map.empty))
      }).publishReplay(1).refCount
    val enhancedSkills: Observable[Map[Int,SkillInfo]] = enhancements.flatMap { es =>
      Observable.combineLatest(es.toList.map { case (k,v) =>
        Data.get[SkillInfo](s"pickle/skill/${v.newSkill}.pickle").map(d => (v.oldSkill,d))
      }).map(_.toMap)
    }.publishReplay(1).refCount

    val unitEntry: Observable[Option[UnitEntry]] = unitInfo.map {
      _.fold(Option.empty[UnitEntry])(
        _.entries.values.toList.sortBy(_.rarity).lastOption)
    }.publishReplay(1).refCount

    val unitSkills = unitInfo.flatMap { u =>
      Observable.combineLatest(u.fold(
        List.empty[Observable[(UnitSkill, SkillInfo)]])(_.skills.map { s =>
        Data.get[SkillInfo](s"pickle/skill/${s.id}.pickle").map { s -> _ }
      }))
    }.publishReplay(1).refCount

    val esperIdSubject = Subject[Option[String]]()
    val esperStats = createHandler[Option[EsperStatInfo]](None)
    val esperSkills = createHandler[List[(String,List[String],List[SkillEffect])]](Nil)
    val esper = createHandler[Option[EsperData]](None)
    val esperEntry = createHandler[Option[EsperEntry]](None)

    def equipFor(idOb: Observable[Option[String]]): Observable[Option[EquipIndex]] = for {
      ms <- equips
      id <- idOb
    } yield {
      ms.find(_.id == id.flatMap(i => util.Try(i.toInt).toOption).getOrElse(0))
    }
    val rhandId = createIdHandler(None)
    val rhandSubject = Subject[Option[String]]()
    val rhand = equipFor(rhandId.merge(rhandSubject))
    val lhandId = createIdHandler(None)
    val lhandSubject = Subject[Option[String]]()
    val lhand = equipFor(lhandId.merge(lhandSubject))
    val headId = createIdHandler(None)
    val headSubject = Subject[Option[String]]()
    val headEquip = equipFor(headId.merge(headSubject))
    val bodyId = createIdHandler(None)
    val bodySubject = Subject[Option[String]]()
    val bodyEquip = equipFor(bodyId.merge(bodySubject))
    val acc1Id = createIdHandler(None)
    val acc1Subject = Subject[Option[String]]()
    val acc1 = equipFor(acc1Id.merge(acc1Subject))
    val acc2Id = createIdHandler(None)
    val acc2Subject = Subject[Option[String]]()
    val acc2 = equipFor(acc2Id.merge(acc2Subject))

    val equippedGear = withStamp(rhand).combineLatest(
      withStamp(lhand), withStamp(headEquip), withStamp(bodyEquip))
    val accs = withStamp(acc1).combineLatest(withStamp(acc2))

    val unitStats = createHandler[Option[BaseStats]](None)
    val selectedTraits = createHandler[List[SkillInfo]]()
    val unitPassives = selectedTraits.map(_.flatMap(_.skilleffects))
    val _sorting = createHandler[Sort](Sort.AZ)
    val sorting = _sorting.publishReplay(1).refCount
    val abilitySubjects = AbilitySubjects()
    val (ability1, ability2, ability3, ability4, abilitySlots) =
      components.abilitySlots(materia, unitInfo, unitPassives, unitEntry, sorting, abilitySubjects)
    val abilities = withStamp(ability1).combineLatest(
      withStamp(ability2), withStamp(ability3),
      withStamp(ability4)).map(Abilities.tupled.apply)

    val equipped = equippedGear.combineLatest(accs).map { a =>
      Equipped.tupled.apply(a._1 + a._2)
    }.combineLatest(abilities)

    def passivesFromAll(equips: List[EquipIndex],
      abilities: List[MateriaIndex]) : List[SkillEffect] = {
      passivesFromEq(equips) ++ passivesFromMat(abilities)
    }
    def skillsFromAll(equips: List[EquipIndex],
      abilities: List[MateriaIndex]): List[(String,String)] = {
      skillsFromEq(equips) ++ skillsFromMat(abilities)
    }

    def passivesFromEq(equip: List[EquipIndex]) = equip.flatMap(_.skilleffects)
    def passivesFromMat(equip: List[MateriaIndex]) =
      equip.flatMap(_.skilleffects)
    def skillsFromEq(equip: List[EquipIndex]) =
      equip.flatMap(e =>
        e.skillEffects.toList.map { case (k,v) => k -> v.mkString("\n") }
      )
    def skillsFromMat(equip: List[MateriaIndex]) =
      equip.flatMap(m => List(m.name -> m.effects.mkString("\n")))

    def typeOf(eqItem: Option[EquipIndex]): Int =
      eqItem.fold(-1)(_.tpe)
    def isSlot(slot: Int, eqItem: Option[EquipIndex]): Boolean =
      eqItem.exists(_.slotId == slot)

    val allPassives = unitInfo.combineLatest(unitPassives, equipped, esperSkills).map {
      case (info, passives,(eqs,abis), fromEsper) =>
      info -> SkillEffect.collateEffects(info, passivesFromAll(eqs.allEquipped, abis.allEquipped) ++ passives ++ fromEsper.flatMap(_._3))
    }

    val equipSkills: Observable[List[(String,String)]] = equipped.combineLatest(esperSkills).map {
      case ((eqs, abis), fromE) =>
      skillsFromAll(eqs.allEquipped, abis.allEquipped) ++
        fromE.map { case (n,d,e) => n -> d.mkString("\n") }
    }

    def publishTo[A](sink: Subject[A], value: A): Unit = sink.next(value)
    def handValidator(
      r: Option[EquipIndex], l: Option[EquipIndex],
      info: Option[UnitData], effs: SkillEffect.CollatedEffect,
      sink: Subject[Option[String]], older: Boolean): String = {
      if (isSlot(2, r) && isSlot(2, l) && older) {
        publishTo(sink, None)
        EMPTY
      } else if ((isSlot(1, r) && isSlot(1, l)) &&
        ((!effs.canDualWield(typeOf(r)) || !effs.canDualWield(typeOf(l))) && older)) {
        publishTo(sink, None)
        EMPTY
      } else if (r.nonEmpty && !effs.canEquip(typeOf(r), info)) {
        publishTo(sink, None)
        EMPTY
      } else r.fold(EMPTY)(_.id.toString)
    }

    def equipValidator(
      e: Option[EquipIndex],
      info: Option[UnitData],
      effs: SkillEffect.CollatedEffect, sink: Subject[Option[String]]): String = {
      if (e.nonEmpty && !effs.canEquip(typeOf(e), info)) {
        publishTo(sink, None)
        EMPTY
      } else e.fold(EMPTY)(_.id.toString)
    }

    val rhandValidator = allPassives.combineLatest(equipped).map {
      case (((info,effs),(eqs, abis))) =>
        handValidator(eqs.rhand._1, eqs.lhand._1, info, effs, rhandSubject, eqs.rhand._2 < eqs.lhand._2)
    }
    val lhandValidator = allPassives.combineLatest(equipped).map {
      case (((info,effs),(eqs, abis))) =>
        handValidator(eqs.lhand._1, eqs.rhand._1, info, effs, lhandSubject, eqs.lhand._2 < eqs.rhand._2)
    }
    def equipsValidator(sink: Subject[Option[String]], f: Equipped => EqStamp) = allPassives.combineLatest(equipped).map {
      case (((info,effs),(eqs, abis))) =>
        equipValidator(f(eqs)._1, info, effs, sink)
    }

    def enhancementsOf(id: Int, enhs: Map[Int,SkillInfo]):
    Option[(SkillInfo,SkillInfo)] =
      for {
        p1 <- enhs.get(id)
        p2 <- enhs.get(p1.id)
      } yield (p1, p2)

    def enhancedInfo[A](info: SkillInfo, enhanced: Option[Int], enhs: Map[Int,SkillInfo], f: SkillInfo => A): A = {
      enhanced.fold(f(info)) { en =>
        val d = enhs(info.id)
        val s = if (en == info.id) info else if (d.id == en) d else enhs(enhs(info.id).id)
        f(s)
      }
    }
    def deco[A,B,C](f: (A,B,C) => VNode): ((A,B,C)) => VNode = f.tupled(_)
    val activesTable = {
      val enhSink = createHandler[(Int,Int)]()
      val enhMap = enhSink.scan(Map.empty[Int,Int]) { (ac, e) =>
        ac + e
      }.startWith(Map.empty)

      unitSkills.combineLatest(enhancedSkills).map(a => a._1.filter(_._2.active).map(b => (b._1, b._2, a._2))).map { ss =>
        components.dataTable(ss,
          "skills-active",
          List("Rarity", "Level", "Name", "Description", "MP"),
          List("unit-skill-rarity", "unit-skill-level",
            "unit-skill-name", "unit-skill-desc", "unit-skill-cost"))(
          List(
            a => span(s"${a._1.rarity}\u2605"),
            a => span(a._1.level.toString),
            deco { (us, info, enhs) =>
              enhancementsOf(info.id, enhs).fold {
                span(info.name)
              } { enh =>
                select(inputString(i => info.id -> i.toInt) --> enhSink,
                  option(value := info.id, info.name),
                  option(value := enh._1.id, "+1 " + info.name),
                  option(value := enh._2.id, "+2 " + info.name)
                )
              }
            },
            deco { (us, info, enhs) =>
              div(children <-- enhMap.map(e => enhancedInfo(info, e.get(info.id), enhs, _.effects.map(e => div(e)))))
            },
            deco { (us, info, enhs) =>
              span(child <-- enhMap.map(e => enhancedInfo(info, e.get(info.id), enhs, _.mpCost.toString)))
            }
          )
        )
      }
    }

    val traitsTable = {
      val enhSink = createHandler[(Int,Int)]()
      val enhMap = enhSink.scan(Map.empty[Int,Int]) { (ac, e) =>
        ac + e
      }.startWith(Map.empty)

      unitSkills.combineLatest(enhancedSkills).map(a => a._1.filterNot(_._2.active).map(b => (b._1, b._2, a._2))).map { ss =>
        val infos = ss.map(_._2).toList
        val enhs = ss.headOption.fold(Map.empty[Int,SkillInfo])(_._3)
        selectedTraits <-- enhMap.map { es =>
          infos.map { i =>
            val rid = es.getOrElse(i.id, i.id)
            if (rid == i.id) i
            else {
              val si = enhs(i.id)
              if (si.id == rid) si
              else enhs(si.id)
            }
          }
        }
        components.dataTable(ss,
          "skills-trait",
          List("Rarity", "Level", "Name", "Description"),
          List("unit-trait-rarity", "unit-trait-level",
            "unit-trait-name", "unit-trait-desc"))(List(
            a => span(s"${a._1.rarity}\u2605"),
            a => span(a._1.level.toString),
            deco { (us, info, enhs) =>
              enhancementsOf(info.id, enhs).fold {
                span(info.name)
              } { enh =>
                select(inputString(i => info.id -> i.toInt) --> enhSink,
                  option(value := info.id, info.name),
                  option(value := enh._1.id, "+1 " + info.name),
                  option(value := enh._2.id, "+2 " + info.name)
                )
              }
            },
            deco { (us, info, enhs) =>
              div(children <-- enhMap.map(e => enhancedInfo(info, e.get(info.id), enhs, _.effects.map(e => div(e)))))
            },
          )
        )
      }
    }

    val equippedTable = equipSkills.map(es => components.dataTable(es,
      "skills-equip",
      List("Name", "Description"),
      List("unit-equip-name", "unit-equip-desc"))(List(
        a => div(a._1.split("\n").map(e => div(e)):_*),
        a => div(a._2.split("\n").map(e => div(e)):_*)
      ))
    )

    val unitDescription = unitInfo.map { i =>
      i.fold("")(_.entries.values.toList.sortBy(
        _.rarity).lastOption.fold("Unknown")(_.strings.description.getOrElse("Unknown")))
    }

    def effectiveStats(u: UnitData, base: BaseStats, equip: EquipIndex, pasv: SkillEffect.CollatedEffect): Stats = {
      val eqs = equip.stats
      val elements = eqs.element.fold(List.empty[Int])(_.map(e =>
        SkillEffect.ELEMENTS.getOrElse(e, -1)))
      val elestats = elements.map(e =>
        pasv.weapEleStats.getOrElse(e, PassiveStatEffect.zero))
      val eqstats = pasv.equipStats.getOrElse(equip.tpe, PassiveStatEffect.zero)
      val s = Stats.fromEquipStats(equip.stats)
      val innates = SkillEffect.collateEffects(Some(u), equip.skilleffects)

      val innatestats = innates.stats :: innates.equipStats.keys.toList.flatMap {
        k => if (pasv.canEquip(k, Some(u))) List(innates.equipStats(k)) else Nil
      }
      (eqstats :: (innatestats ++ elestats)).foldLeft(s) { (ac, x) =>
        ac + base.asStats * x - base.asStats
      }
    }
    def sortFor(xs: List[EquipIndex], sorting: Sort, pasv: SkillEffect.CollatedEffect, unit: Option[UnitData], base: Option[BaseStats]) = {
      val m = for {
        u <- unit
        b <- base
      } yield {
        val es = effectiveStats(u, b, _: EquipIndex, pasv)
        def cmp(f: Stats => Int):
          (EquipIndex,EquipIndex) => Boolean = (x,y) => f(es(x)) > f(es(y))
        val f: (EquipIndex,EquipIndex) => Boolean = sorting match {
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

    def equippable(slots: Set[Int], worn: Observable[Option[EquipIndex]]) = for {
      ((es, (u, passives), sort, base), w) <- equips.combineLatest(allPassives, sorting, unitStats).combineLatest(worn)
    } yield {
      val eqs = es.filter(e =>
        slots(e.slotId) && e.canEquip(u) && passives.canEquip(e.tpe, u))

      List(option(value := EMPTY, "Empty")) ++
        sortFor(eqs, sort, passives, u, base).map { e =>
          option(value := e.id,
            selected := w.exists(_.id == e.id),
            s"${e.name} \u27a1 ${e.stats} ${e.describeEffects(u)}")
        }
    }

    def eqslot(name: String, slots: Set[Int], validator: Observable[String], worn: Observable[Option[EquipIndex]], sink: outwatch.Sink[Option[String]]): VNode = {
      td(label(name, select(cls := "equip-slot",
        value <-- validator,
        children <-- equippable(slots, worn),
        inputId --> sink)))
    }

    val esperTraining = createHandler[Map[Int,Boolean]](Map.empty)
    val esperTrainingSubject = Subject[Map[Int,Boolean]]()
    val esperRarity = createStringHandler("1")

    def subscribeChanges = equipped.combineLatest(unitStats,unitInfo).combineLatest(espers, esper).combineLatest(esperRarity, esperTraining).bufferTime(1.second).map(_.lastOption) {
      case Some((((((eqs,abis),sts,i),es, e)),rarity, training)) =>
      /*
        val ps = PageState.from(
          i.map(_.id), sts, eqs, abis, e.map(x => es(x.names.head)),
          util.Try(rarity.toInt).getOrElse(1), training)
        if (i.nonEmpty) {
          val pstr = Data.toString(ps)
          if (document.location.hash.drop(1) != pstr)
            window.history.pushState(0, i.fold("ffbecalc")(_.name), "#" + pstr)
        }
        */
        val update = i.map { u =>
          e.fold(u.id.toString)(esp => u.id.toString + "," + es(esp.names.head))
        }

        if (document.location.hash != update.getOrElse(""))
          document.location.hash = update.getOrElse("")
      case None =>
      }

    def loadFromHash(): Unit = {
      val hash = document.location.hash.drop(1).split(",")
      val unitid = hash.headOption.filter(_.nonEmpty)

      unitIdSubject.next(unitid)
      if (hash.size > 1)
        esperIdSubject.next(hash.lastOption.filter(_.nonEmpty))

      /*
      val hash = document.location.hash.drop(1)
      val ps = util.Try(Data.fromString[PageState](hash)).toOption
      def s(i: Option[Int]): Option[String] = i.map(_.toString)
      for {
        state <- ps
        unit <- state.unit
      } {
        unitIdSubject.next(Some(unit.toString))
        pots.next(state.pots)
        abilitySubjects.a1.next(s(state.mat1))
        abilitySubjects.a2.next(s(state.mat2))
        abilitySubjects.a3.next(s(state.mat3))
        abilitySubjects.a4.next(s(state.mat4))
        acc1Subject.next(s(state.acc1))
        acc2Subject.next(s(state.acc2))
        headSubject.next(s(state.head))
        bodySubject.next(s(state.body))
        rhandSubject.next(s(state.rhand))
        lhandSubject.next(s(state.lhand))
        esperIdSubject.next(s(state.esper))
        esperTrainingSubject.next(state.esperSkills)
      }
      */
    }

    val onLoad = outwatch.Sink.create[org.scalajs.dom.raw.Element] { e =>
      var subscription = Option.empty[rxscalajs.subscription.Subscription]
      window.addEventListener("popstate",
        { e: org.scalajs.dom.PopStateEvent =>
          subscription.foreach(_.unsubscribe)
          loadFromHash()
          subscription = Some(subscribeChanges)
        }, true)
      loadFromHash()
      subscription = Some(subscribeChanges)
    }

    OutWatch.render("#content",
      div(insert --> onLoad,
        div(id := "unit-info",
          select(children <-- unitIndex, inputId --> unitIdSink),
          div(hidden <-- unitId.map(_.isEmpty),
            components.unitBaseStats(unitEntry, unitStats, pots),
            components.unitStats(unitInfo, unitEntry, unitStats, equipped, allPassives.map(_._2), esperStats, esperEntry),
          )
        ),
        div(hidden <-- unitId.map(_.isEmpty),
        p(child <-- unitDescription.orElse(Observable.just(""))),
        h3("Equipment"),
        components.sortBy(_sorting),
        table(id := "equip-slots",
          tr(
            eqslot("Right Hand", Set(1, 2), rhandValidator, rhand, rhandId),
            eqslot("Left Hand",  Set(1, 2), lhandValidator, lhand, lhandId),
          ),
          tr(
            eqslot("Head", Set(3), equipsValidator(headSubject, _.head), headEquip, headId),
            eqslot("Body", Set(4), equipsValidator(bodySubject, _.body), bodyEquip, bodyId),
          ),
          tr(
            eqslot("Accessory 1", Set(5), equipsValidator(acc1Subject, _.acc1), acc1, acc1Id),
            eqslot("Accessory 2", Set(5), equipsValidator(acc2Subject, _.acc2), acc2, acc2Id),
          )
        ),
        h3("Materia"),
        table(id := "materia-slots",
          children <-- abilitySlots,
        ),
        h3("Esper"),
        Esper.esperInfo(esper, esperEntry, esperRarity, espers, esperIdSubject, esperStats, esperSkills, esperTraining, esperTrainingSubject),
        h3("Abilities & Spells"),
        div(child <-- activesTable),
        h3("Traits"),
        div(child <-- traitsTable),
        div(hidden <-- equipSkills.map(_.isEmpty),
          h3("Equipped"),
          div(child <-- equippedTable),
        ),
        ),
      )
    )
  }
}
