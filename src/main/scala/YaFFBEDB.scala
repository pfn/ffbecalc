package yaffbedb

import scala.scalajs.js.JSApp
import org.scalajs.dom.document

import outwatch.dom._
import rxscalajs.{Observable,Subject}
import boopickle.Default._

object YaFFBEDB extends JSApp {
  def main(): Unit = {
    val idx = Data.get[List[UnitIndex]]("pickle/unit/index.pickle").map { us =>
      List(option(value := EMPTY, "-- Select a unit --")) ++
        us.map { u =>
          val rarity = ("\u2605" * u.min) + ("\u2606" * (u.max - u.min))
          option(value := u.id, s"${u.name}: $rarity")
        }
    }

    val equips  = Data.get[List[EquipIndex]]("pickle/equip/index.pickle")
    val materia = Data.get[List[MateriaIndex]]("pickle/materia/index.pickle")
    val espers  = Data.get[Map[String,Int]]("pickle/esper/index.pickle")

    val unitIdSink = createStringHandler()
    val unitIdSubject = Subject[Option[String]]
    val unitId = unitIdSubject.merge(unitIdSink.map(maybeId))

    val unitInfo: Observable[Option[UnitData]] = unitId.flatMap(id => 
      id.fold(Observable.just(Option.empty[UnitData])) { id_ =>
        Data.get[UnitData](s"pickle/unit/$id_.pickle").map { u =>
          Some(u)
        }
      })

    val unitEntry: Observable[Option[UnitEntry]] = unitInfo.map {
      _.fold(Option.empty[UnitEntry])(
        _.entries.values.toList.sortBy(_.rarity).lastOption)
    }

    val unitSkills = unitInfo.flatMap { u =>
      Observable.combineLatest(u.fold(
        List.empty[Observable[(UnitSkill, SkillInfo)]])(_.skills.map { s =>
        Data.get[SkillInfo](s"pickle/skill/${s.id}.pickle").map { s -> _ }
      }))
    }

    val esperIdSubject = Subject[Option[String]]()
    val esperStats = createHandler[Option[EsperStatInfo]]()
    val (esper, esperInfo) = Esper.esperInfo(espers, esperIdSubject, esperStats)

    def equipFor(idOb: Observable[Option[String]]): Observable[Option[EquipIndex]] = for {
      ms <- equips
      id <- idOb
    } yield ms.find(_.id == id.flatMap(i => util.Try(i.toInt).toOption).getOrElse(0))
    val onLoad = outwatch.Sink.create[org.scalajs.dom.raw.Element] { e =>
      val hash = document.location.hash.drop(1).split(",")
      val unitid = hash.headOption

      unitIdSubject.next(unitid)
      if (hash.size > 1)
        esperIdSubject.next(hash.lastOption)
    }

    espers.combineLatest(esper.startWith(None), unitId) { case (es, e,i) =>
      val update = i.map { id =>
        e.fold(id)(esp => id + "," + es(esp.names.head))
      }
      document.location.hash = update.getOrElse("")
    }

    val rhandSink = createStringHandler()
    val rhandId = prependNone(rhandSink)
    val rhandSubject = Subject[Option[String]]()
    val rhand = equipFor(rhandId.merge(rhandSubject))
    val lhandSink = createStringHandler()
    val lhandId = prependNone(lhandSink)
    val lhandSubject = Subject[Option[String]]()
    val lhand = equipFor(lhandId.merge(lhandSubject))
    val headSink = createStringHandler()
    val headSubject = Subject[Option[String]]()
    val headId = prependNone(headSink)
    val headEquip = equipFor(headId.merge(headSubject))
    val bodySink = createStringHandler()
    val bodySubject = Subject[Option[String]]()
    val bodyId = prependNone(bodySink)
    val bodyEquip = equipFor(bodyId.merge(bodySubject))
    val acc1Sink = createStringHandler()
    val acc1Subject = Subject[Option[String]]()
    val acc1Id = prependNone(acc1Sink)
    val acc1 = equipFor(acc1Id.merge(acc1Subject))
    val acc2Sink = createStringHandler()
    val acc2Subject = Subject[Option[String]]()
    val acc2Id = prependNone(acc2Sink)
    val acc2 = equipFor(acc2Id.merge(acc2Subject))

    val equippedGear = withStamp(rhand).combineLatest(
      withStamp(lhand), withStamp(headEquip), withStamp(bodyEquip))
    val accs = withStamp(acc1).combineLatest(withStamp(acc2))

    type EqStamp = (Option[EquipIndex],Double)
    type MatStamp = (Option[MateriaIndex],Double)
    case class Equipped(
      rhand: EqStamp, lhand: EqStamp,
      head:  EqStamp, body:  EqStamp,
      acc1:  EqStamp, acc2:  EqStamp) {
      def allEquipped: List[EquipIndex] =
        (rhand._1 ++ lhand._1 ++ head._1 ++ body._1 ++ acc1._1 ++ acc2._1).toList
    }
    case class Abilities(
      ability1: MatStamp, ability2: MatStamp,
      ability3: MatStamp, ability4: MatStamp) {
      def allEquipped: List[MateriaIndex] =
        (ability1._1 ++ ability2._1 ++ ability3._1 ++ ability4._1).toList
    }

    val (ability1, ability2, ability3, ability4, abilitySlots) =
      components.abilitySlots(materia, unitInfo, unitEntry)
    val abilities = withStamp(ability1).combineLatest(
      withStamp(ability2), withStamp(ability3),
      withStamp(ability4)).map(Abilities.tupled.apply)

    val equipped = equippedGear.combineLatest(accs).map { a =>
      Equipped.tupled.apply(a._1 + a._2)
    }.combineLatest(abilities)

    val equippedStats = equipped.map { case (eqs, abis) =>
      ()
    }

    def passivesFromAll(equips: List[EquipIndex],
      abilities: List[MateriaIndex]) : List[SkillEffect] = {
      passivesFromEq(equips) ++ passivesFromMat(abilities)
    }
    def skillsFromAll(equips: List[EquipIndex],
      abilities: List[MateriaIndex]): List[(String,String)] = {
      skillsFromEq(equips) ++ skillsFromMat(abilities)
    }

    def passivesFromEq(equip: List[EquipIndex]) =
      equip.flatMap(_.skilleffects)
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

    val unitPassives = unitSkills.map { _.filterNot(_._2.active).flatMap {
      case (_,info) => info.skilleffects
    }}
    val allPassives = unitInfo.combineLatest(unitPassives, equipped).map {
      case (info, passives,(eqs,abis)) =>
      info -> SkillEffect.collateEffects(info, passivesFromAll(eqs.allEquipped, abis.allEquipped) ++ passives)
    }

    val equipSkills: Observable[List[(String,String)]] = equipped.map {
      case (eqs, abis) =>
      skillsFromAll(eqs.allEquipped, abis.allEquipped)
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

    val activesTable = components.dataTable(unitSkills.map(_.filter(_._2.active)),
      "skills-active",
      List("Rarity", "Level", "Name", "Description", "MP"),
      List("unit-skill-rarity", "unit-skill-level", "unit-skill-name", "unit-skill-desc", "unit-skill-cost"))(
      List(
        a => span(s"${a._1.rarity}\u2605"),
        a => span(a._1.level.toString),
        a => span(a._2.name),
        a => div(a._2.effects.map(e => div(e)): _*),
        a => span(a._2.mpCost.toString)
      ))

    val traitsTable = components.dataTable(unitSkills.map(_.filterNot(_._2.active)),
      "skills-trait",
      List("Rarity", "Level", "Name", "Description"),
      List("unit-trait-rarity", "unit-trait-level", "unit-trait-name", "unit-trait-desc"))(List(
        a => span(s"${a._1.rarity}\u2605"),
        a => span(a._1.level.toString),
        a => span(a._2.name),
        a => div(a._2.effects.map(e => div(e)): _*),
      ))

    val equippedTable = components.dataTable(equipSkills,
      "skills-equip",
      List("Name", "Description"),
      List("unit-equip-name", "unit-equip-desc"))(List(
        a => div(a._1.split("\n").map(e => div(e)):_*),
        a => div(a._2.split("\n").map(e => div(e)):_*)
      ))

    val unitDescription = unitInfo.map { i =>
      i.fold("")(_.entries.values.toList.sortBy(
        _.rarity).lastOption.fold("Unknown")(_.strings.description.getOrElse("Unknown")))
    }

    def equippable(slots: Set[Int]) = for {
      (es, (u, passives)) <- equips.combineLatest(allPassives)
    } yield {
      List(option(value := EMPTY, "Empty")) ++
        es.filter(e => slots(e.slotId) && e.canEquip(u) && passives.canEquip(e.tpe, u)).map { e =>
          option(value := e.id,
            s"${e.name} \u27a1 ${e.stats} ${e.describeEffects(u)}")
        }
    }
    val unitStats = createHandler[Option[Stats]]()

    OutWatch.render("#content",
      div(insert --> onLoad,
        div(id := "unit-info",
          select(children <-- idx, value <-- idx.combineLatest(unitIdSubject).map(_._2).map(_.getOrElse(EMPTY)).startWith(EMPTY), inputString --> unitIdSink),
          div(hidden <-- unitId.map(_.isEmpty).startWith(true),
            components.unitBaseStats(unitEntry, unitStats),
            components.unitStats(unitStats, esperStats),
          )
        ),
        div(hidden <-- unitId.map(_.isEmpty).startWith(true),
        p(child <-- unitDescription.orElse(Observable.just(""))),
        h3("Equipment"),
        table(
          tr(
            td(label(forId := "r-hand", "Right Hand"), select(id := "r-hand", cls := "equip-slot", value <-- rhandValidator, children <-- equippable(Set(1, 2)), inputString --> rhandSink)),
            td(label(forId := "l-hand",  "Left Hand"), select(id := "l-hand", cls := "equip-slot", value <-- lhandValidator, children <-- equippable(Set(1, 2)), inputString --> lhandSink))
          ),
          tr(
            td(label(forId := "u-head", "Head"), select(id := "u-head", cls := "equip-slot", value <-- equipsValidator(headSubject, _.head), children <-- equippable(Set(3)), inputString --> headSink)),
            td(label(forId := "u-body", "Body"), select(id := "u-body", cls := "equip-slot", value <-- equipsValidator(bodySubject, _.body), children <-- equippable(Set(4)), inputString --> bodySink)),
          ),
          tr(
            td(label(forId := "u-acc1", "Accessory 1"), select(id := "u-acc1", cls := "equip-slot", value <-- equipsValidator(acc1Subject, _.acc1), children <-- equippable(Set(5)), inputString --> acc1Sink)),
            td(label(forId := "u-acc2", "Accessory 2"), select(id := "u-acc2", cls := "equip-slot", value <-- equipsValidator(acc2Subject, _.acc2), children <-- equippable(Set(5)), inputString --> acc2Sink))
          )
        ),
        h3("Materia"),
        table(
          children <-- abilitySlots,
        ),
        h3("Esper"),
        esperInfo,
        h3("Abilities & Spells"),
        activesTable,
        h3("Traits"),
        traitsTable,
        div(hidden <-- equipSkills.map(_.isEmpty),
          h3("Equipped"),
          equippedTable,
        ),
        ),
      )
    )
  }
}
