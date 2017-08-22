package yaffbedb

import scala.scalajs.js.JSApp

import outwatch.dom._
import rxscalajs.Observable
import boopickle.Default._

object YaFFBEDB extends JSApp {
  implicit class Tuple4Plus[A,B,C,D](val tuple: (A,B,C,D)) extends AnyVal {
    def +[E,F](other: (E,F)): (A,B,C,D,E,F) =
      (tuple._1, tuple._2, tuple._3, tuple._4, other._1, other._2)
  }
  val EMPTY = "--empty--"
  def main(): Unit = {
    val idx = Data.get[List[UnitIndex]]("pickle/unit/index.pickle").map { us =>
      List(option(value := EMPTY, "-- Select a unit --")) ++
        us.map { u =>
          val rarity = ("\u2605" * u.min) + ("\u2606" * (u.max - u.min))
          option(value := u.id, s"${u.name}: $rarity")
        }
    }

    val equips = Data.get[List[EquipIndex]]("pickle/equip/index.pickle")
    val materia = Data.get[List[MateriaIndex]]("pickle/materia/index.pickle")
    val espers = Data.get[Map[String,Int]]("pickle/esper/index.pickle")

    def maybeId(id: String): Option[String] =
      if (id.startsWith("--")) None else Some(id)
    val unitIdSink = createStringHandler()
    val unitId = unitIdSink.map(maybeId)

    val unitInfo: Observable[Option[UnitData]] = unitId.flatMap(id => 
      id.fold(Observable.just(Option.empty[UnitData])) { id_ =>
        Data.get[UnitData](s"pickle/unit/$id_.pickle").map(Some.apply)
      })

    val unitSkills = unitInfo.flatMap { u =>
      Observable.combineLatest(u.fold(
        List.empty[Observable[(UnitSkill, SkillInfo)]])(_.skills.map { s =>
        Data.get[SkillInfo](s"pickle/skill/${s.id}.pickle").map { s -> _ }
      }))
    }

    val esperSink = createStringHandler()
    val esperId = esperSink.map(maybeId)
    val esper: Observable[Option[EsperData]] = esperId.flatMap { e =>
      e.fold(Observable.just(Option.empty[EsperData])) { eid =>
        Data.get[EsperData](s"pickle/esper/${eid}.pickle").map(Some.apply)
      }
    }

    def prependNone(idOb: Observable[String]): Observable[Option[String]] =
      Observable.just(None) ++ idOb.map(maybeId)
    def materiaFor(idOb: Observable[Option[String]]): Observable[Option[MateriaIndex]] = for {
      ms <- materia
      id <- idOb
    } yield ms.find(_.id == id.flatMap(i => util.Try(i.toInt).toOption).getOrElse(0))
    def equipFor(idOb: Observable[Option[String]]): Observable[Option[EquipIndex]] = for {
      ms <- equips
      id <- idOb
    } yield ms.find(_.id == id.flatMap(i => util.Try(i.toInt).toOption).getOrElse(0))
    def withTimestamp[A](ob: Observable[A]): Observable[(A,Double)] =
      ob.map(_ -> scalajs.js.Date.now())

    val ability1Sink = createStringHandler()
    val ability1Id = prependNone(ability1Sink)
    val ability1 = materiaFor(ability1Id)
    val ability2Sink = createStringHandler()
    val ability2Id = prependNone(ability2Sink)
    val ability2 = materiaFor(ability2Id)
    val ability3Sink = createStringHandler()
    val ability3Id = prependNone(ability3Sink)
    val ability3 = materiaFor(ability3Id)
    val ability4Sink = createStringHandler()
    val ability4Id = prependNone(ability4Sink)
    val ability4 = materiaFor(ability4Id)

    val rhandSink = createStringHandler()
    val rhandId = prependNone(rhandSink)
    val rhand = equipFor(rhandId)
    val lhandSink = createStringHandler()
    val lhandId = prependNone(lhandSink)
    val lhand = equipFor(lhandId)
    val headSink = createStringHandler()
    val headId = prependNone(headSink)
    val headEquip = equipFor(headId)
    val bodySink = createStringHandler()
    val bodyId = prependNone(bodySink)
    val bodyEquip = equipFor(bodyId)
    val acc1Sink = createStringHandler()
    val acc1Id = prependNone(acc1Sink)
    val acc1 = equipFor(acc1Id)
    val acc2Sink = createStringHandler()
    val acc2Id = prependNone(acc2Sink)
    val acc2 = equipFor(acc2Id)

    val equippedGear = withTimestamp(rhand).combineLatest(
      withTimestamp(lhand), withTimestamp(headEquip), withTimestamp(bodyEquip))
    val accs = withTimestamp(acc1).combineLatest(withTimestamp(acc2))

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

    val abilities = withTimestamp(ability1).combineLatest(
      withTimestamp(ability2), withTimestamp(ability3),
      withTimestamp(ability4)).map(Abilities.tupled.apply)

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
      eqItem.fold(-1)(_.slotId) == slot

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

    def clearInput(node: scalajs.js.Dynamic): Unit = {
      val evt = scalajs.js.Dynamic.global.document.createEvent("HTMLEvents")
      evt.initEvent("change", true, true)
      node.value = EMPTY
      node.dispatchEvent(evt)
    }
    def handValidator(node: String,
      r: Option[EquipIndex], l: Option[EquipIndex],
      info: Option[UnitData], effs: SkillEffect.CollatedEffect,
      older: Boolean): Unit = {
      val n = scalajs.js.Dynamic.global.document.getElementById(node)
      var isValid = true
      if (isSlot(2, r) && isSlot(2, r) && older) {
        clearInput(n)
        isValid = false
      }
      if (isSlot(1, r) && isSlot(1, l)) {
        if ((!effs.canDualWield(typeOf(r)) || !effs.canDualWield(typeOf(l))) && older) {
          clearInput(n)
          isValid = false
        }
      }
      if (r.nonEmpty && !effs.canEquip(typeOf(r), info)) {
        clearInput(n)
        isValid = false
      }
      r.foreach { e =>
        if (isValid && e.id.toString != n.value) {
          n.value = e.id
        }
      }
    }

    def equipValidator(
      node: String,
      e: Option[EquipIndex],
      info: Option[UnitData],
      effs: SkillEffect.CollatedEffect): Unit = {
      val n = scalajs.js.Dynamic.global.document.getElementById(node)
      var isValid = true
      if (e.nonEmpty && !effs.canEquip(typeOf(e), info)) {
        clearInput(n)
        isValid = false
      }
      e.foreach { i =>
        if (isValid && i.id.toString != n.value) {
          n.value = i.id
        }
      }
    }

    val rhandValidator = allPassives.combineLatest(equipped).map {
      case (((info,effs),(eqs, abis))) =>
        handValidator("r-hand", eqs.rhand._1, eqs.lhand._1, info, effs, eqs.rhand._2 < eqs.lhand._2)
        EMPTY
    }
    val lhandValidator = allPassives.combineLatest(equipped).map {
      case (((info,effs),(eqs, abis))) =>
        handValidator("l-hand", eqs.lhand._1, eqs.rhand._1, info, effs, eqs.lhand._2 < eqs.rhand._2)
        EMPTY
    }
    val equipsValidator = allPassives.combineLatest(equipped).map {
      case (((info,effs),(eqs, abis))) =>
        equipValidator("u-head", eqs.head._1, info, effs)
        equipValidator("u-body", eqs.body._1, info, effs)
        equipValidator("u-acc1", eqs.acc1._1, info, effs)
        equipValidator("u-acc2", eqs.acc2._1, info, effs)
        EMPTY
    }

    val unitEntry: Observable[Option[UnitEntry]] = unitInfo.map {
      _.fold(Option.empty[UnitEntry])(
        _.entries.values.toList.sortBy(_.rarity).lastOption)
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

    def materiaOption(u: Option[UnitData], e: Option[UnitEntry]): Observable[List[VNode]] =
      materia.map { m =>
        List(option(value := EMPTY, "Empty")) ++
          m.filter(mi => e.exists(_.canEquip(mi))).map { mi =>
            val mid = mi.describeEffects(u)
            val mids = if (mid.trim.isEmpty) ""
            else s"\u27a1 $mid"
            option(value := mi.id, s"${mi.name} $mids")
          }
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

    val abilitySlots = unitInfo.combineLatest(unitEntry).map { case(u, e) =>
      val slots = e.fold(0)(_.abilitySlots)

      if (slots == 0) {
        Nil
      } else if (slots == 1) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", value <-- rhandValidator, children <-- materiaOption(u, e), inputString --> ability1Sink))))
      } else if (slots == 2) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability1Sink)),
          td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability2Sink))))
      } else if (slots == 3) {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability1Sink)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability2Sink))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability3Sink))))
      } else {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability1Sink)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability2Sink))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability3Sink)),
            td(label(forLabel := "u-ability4", "Ability 4"), select(id := "u-ability4", cls := "equip-slot", children <-- materiaOption(u, e), inputString --> ability4Sink))))
      }
    }

    OutWatch.render("#content",
      div(
        div(id := "unit-info",
          select(children <-- idx, inputString --> unitIdSink),
          div(hidden <-- unitId.map(_.isEmpty).startWith(true),
            components.unitStats(unitEntry),
          )
        ),
        div(hidden <-- unitId.map(_.isEmpty).startWith(true),
        p(child <-- unitDescription.orElse(Observable.just(""))),
        h3("Equipment"),
        table(
          tr(
            td(label(forLabel := "r-hand", "Right Hand"), select(id := "r-hand", cls := "equip-slot", children <-- equippable(Set(1, 2)), inputString --> rhandSink)),
            td(label(forLabel := "l-hand", "Left Hand"), select(id := "l-hand", cls := "equip-slot", children <-- equippable(Set(1, 2)), inputString --> lhandSink))
          ),
          tr(
            td(label(forLabel := "u-head", "Head"), select(id := "u-head", cls := "equip-slot", children <-- equippable(Set(3)), inputString --> headSink)),
            td(label(forLabel := "u-body", "Body"), select(id := "u-body", cls := "equip-slot", children <-- equippable(Set(4)), inputString --> bodySink)),
          ),
          tr(
            td(label(forLabel := "u-acc1", "Accessory 1"), select(id := "u-acc1", cls := "equip-slot", children <-- equippable(Set(5)), inputString --> acc1Sink)),
            td(label(forLabel := "u-acc2", "Accessory 2"), select(id := "u-acc2", cls := "equip-slot", children <-- equippable(Set(5)), inputString --> acc2Sink))
          )
        ),
        h3("Materia"),
        table(
          children <-- abilitySlots,
        ),
        h3("Esper"),
        div(id := "esper-container",
          select(children <-- espers.map { es =>
            val names = es.keys.toList.sorted
            option(value := EMPTY, "-- Select Esper --") ::
              names.map(n => option(value := es(n), n))
          }, inputString --> esperSink),
          span(hidden <-- esper.startWith(None).map(_.isEmpty), " ",
            span(child <-- esper.map(_.fold(""){_.entries(1).stats.hp.max + "HP"}))
          )
        ),
        h3("Abilities & Spells"),
        activesTable,
        h3("Traits"),
        traitsTable,
        div(hidden <-- equipSkills.map(_.isEmpty),
          h3("Equipped"),
          equippedTable,
        ),
        ),
        meta(name := "validation-sink-placeholder", content <-- rhandValidator.combineLatest(lhandValidator, equipsValidator).map(_ => "")),
      )
    )
  }
}

object Data {
  import java.nio.ByteBuffer
  import scala.scalajs.js.typedarray.{TypedArrayBuffer,ArrayBuffer}
  import org.scalajs.dom.ext.Ajax
  import scala.concurrent.ExecutionContext.Implicits.global
  def get[A : Pickler](url: String): Observable[A] = {
    Observable.from(Ajax.get(
      url = url,
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(r => Unpickle[A].fromBytes(
      TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))))
  }
}
