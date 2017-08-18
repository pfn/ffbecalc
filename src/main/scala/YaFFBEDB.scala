package yaffbedb

import scala.scalajs.js.JSApp

import outwatch.dom._
import rxscalajs.Observable
import boopickle.Default._

object YaFFBEDB extends JSApp {
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

    val equipsObserver = {
      val start = scalajs.js.Date.now()
      val first = p(start.toString)
      Observable.just(List(first)) ++
        equips.map { _ =>
          val now = scalajs.js.Date.now()
          List(first, p(now.toString), p((now - start).toString))
        }
    }

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

    val unitSkills  = unitInfo.flatMap { u =>
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
    val abilities = withTimestamp(ability1).combineLatest(
      withTimestamp(ability2), withTimestamp(ability3), withTimestamp(ability4))

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
    val equipped = equippedGear.combineLatest(accs).combineLatest(abilities)

    type EqStamp = (Option[EquipIndex],Double)
    type MatStamp = (Option[MateriaIndex],Double)
    def passivesFromAll(
      equips: (((EqStamp, EqStamp, EqStamp, EqStamp),(EqStamp,EqStamp)),
        (MatStamp,MatStamp,MatStamp,MatStamp))): List[SkillEffect] = {
      val ((((r,_),(l,_),(h,_),(b,_)),((a1,_),(a2,_))),((m1,_),(m2,_),(m3,_),(m4,_))) = equips
      passivesFromEq(List(r, l, h, b, a1, a2)) ++
        passivesFromMat(List(m1, m2, m3, m4))
    }
    def skillsFromAll(
      equips: (((EqStamp, EqStamp, EqStamp, EqStamp),(EqStamp,EqStamp)),
        (MatStamp,MatStamp,MatStamp,MatStamp))): List[(String,String)] = {
      val ((((r,_),(l,_),(h,_),(b,_)),((a1,_),(a2,_))),((m1,_),(m2,_),(m3,_),(m4,_))) = equips
      skillsFromEq(List(r, l, h, b, a1, a2)) ++
        skillsFromMat(List(m1, m2, m3, m4))
    }

    def passivesFromEq(equip: List[Option[EquipIndex]]) =
      equip.flatMap(_.fold(List.empty[SkillEffect])(_.skilleffects))
    def passivesFromMat(equip: List[Option[MateriaIndex]]) =
      equip.flatMap(_.fold(List.empty[SkillEffect])(_.skilleffects))
    def skillsFromEq(equip: List[Option[EquipIndex]]) =
      equip.flatMap(_.fold(List.empty[(String,String)])(e =>
        e.skillEffects.toList.map { case (k,v) => k -> v.mkString("\n") }
      ))
    def skillsFromMat(equip: List[Option[MateriaIndex]]) =
      equip.flatMap(_.fold(List.empty[(String,String)])(m => List(m.name -> m.effects.mkString("\n"))))

    def typeOf(eqItem: Option[EquipIndex]): Int =
      eqItem.fold(-1)(_.tpe)
    def isSlot(slot: Int, eqItem: Option[EquipIndex]): Boolean =
      eqItem.fold(-1)(_.slotId) == slot

    val unitPassives = unitSkills.map { _.filterNot(_._2.active).flatMap {
      case (_,info) => info.skilleffects
    }}
    val allPassives = unitInfo.combineLatest(unitPassives, equipped).map {
      case ((info, passives,all@(((_, _, _, _),(_, _)),(_,_,_,_)))) =>
      info -> SkillEffect.collateEffects(passivesFromAll(all) ++ passives)
    }

    val equipSkills: Observable[List[(String,String)]] = equipped.map {
      case (all@(((_, _, _, _),(_, _)),(_,_,_,_))) =>
      skillsFromAll(all)
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
      case (((info,effs),(((rh, lh, hd, bd),(a1, a2)),(m1,m2,m3,m4)))) =>
        handValidator("r-hand", rh._1, lh._1, info, effs, rh._2 < lh._2)
        EMPTY
    }
    val lhandValidator = allPassives.combineLatest(equipped).map {
      case (((info,effs),(((rh, lh, hd, bd),(a1, a2)),(m1,m2,m3,m4)))) =>
        handValidator("l-hand", lh._1, rh._1, info, effs, lh._2 < rh._2)
        EMPTY
    }
    val equipsValidator = allPassives.combineLatest(equipped).map {
      case (((info,effs),(((rh, lh, hd, bd),(a1, a2)),(m1,m2,m3,m4)))) =>
        equipValidator("u-head", hd._1, info, effs)
        equipValidator("u-body", hd._1, info, effs)
        equipValidator("u-acc1", hd._1, info, effs)
        equipValidator("u-acc2", hd._1, info, effs)
        EMPTY
    }

    val unitEntry: Observable[Option[UnitEntry]] = unitInfo.map {
      _.fold(Option.empty[UnitEntry])(
        _.entries.values.toList.sortBy(_.rarity).lastOption)
    }

    val unitActives = unitSkills.map { skills =>
      skills.filter(_._2.active).map { case (unitskill, skillinfo) =>
        tr(
          td(cls := "unit-skill-rarity", s"${unitskill.rarity}\u2605"),
          td(cls := "unit-skill-level", unitskill.level.toString),
          td(cls := "unit-skill-name", skillinfo.name),
          td(cls := "unit-skill-desc", children <--
            Observable.just(skillinfo.effects.map(e => div(e)))),
          td(cls := "unit-skill-cost", skillinfo.mpCost.toString),
        )
      }
    }

    val unitTraits = unitSkills.map { skills =>
      skills.filterNot(_._2.active).map { case (unitskill, skillinfo) =>
        tr(
          td(cls := "unit-trait-rarity", s"${unitskill.rarity}\u2605"),
          td(cls := "unit-trait-level",  unitskill.level.toString),
          td(cls := "unit-trait-name",   skillinfo.name),
          td(cls := "unit-trait-desc",   children <--
            Observable.just(skillinfo.effects.map(e => div(e)))),
        )
      }
    }

    val equippedSkills = equipSkills.map { skills =>
      skills.map { case (name, desc) =>
      tr(
          td(cls := "unit-equip-name",   children <-- Observable.just(name.split("\n").map(e => div(e)))),
          td(cls := "unit-equip-desc",   children <--
            Observable.just(desc.split("\n").map(e => div(e)))),
            )
      }
    }

    val unitDescription = unitInfo.map { i =>
      i.fold("")(_.entries.values.toList.sortBy(
        _.rarity).lastOption.fold("Unknown")(_.strings.description.getOrElse("Unknown")))
    }

    val unitPresenceCheck: (Option[UnitData],EquipIndex) => Boolean = (u, e) =>
      u.nonEmpty

    def materiaOption(e: Option[UnitEntry]): Observable[List[VNode]] =
      materia.map { m =>
        List(option(value := EMPTY, "Empty")) ++
          m.filter(mi => e.exists(_.canEquip(mi))).map { mi =>
            val mid = mi.describeEffects
            val mids = if (mid.trim.isEmpty) ""
            else s"\u27a1 $mid"
            option(value := mi.id, s"${mi.name} $mids")
          }
      }

    def equippable(slots: Set[Int]) = for {
      (es, (u, passives)) <- equips.combineLatest(allPassives)
    } yield {
      List(option(value := EMPTY, "Empty")) ++
        es.filter(e => slots(e.slotId) && passives.canEquip(e.tpe, u)).map { e =>
          option(value := e.id,
            s"${e.name} \u27a1 ${e.stats} ${e.describeEffects}")
        }
    }

    val abilitySlots = unitEntry.map { e =>
      val slots = e.fold(0)(_.abilitySlots)

      if (slots == 0) {
        Nil
      } else if (slots == 1) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", value <-- rhandValidator, children <-- materiaOption(e), inputString --> ability1Sink))))
      } else if (slots == 2) {
        List(tr(td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability1Sink)),
          td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability2Sink))))
      } else if (slots == 3) {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability1Sink)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability2Sink))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability3Sink))))
      } else {
        List(
          tr(
            td(label(forLabel := "u-ability1", "Ability 1"), select(id := "u-ability1", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability1Sink)),
            td(label(forLabel := "u-ability2", "Ability 2"), select(id := "u-ability2", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability2Sink))),
          tr(
            td(label(forLabel := "u-ability3", "Ability 3"), select(id := "u-ability3", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability3Sink)),
            td(label(forLabel := "u-ability4", "Ability 4"), select(id := "u-ability4", cls := "equip-slot", children <-- materiaOption(e), inputString --> ability4Sink))))
      }
    }

    OutWatch.render("#content",
      div(
        p("Loading equips...", children <-- equipsObserver),
        select(children <-- idx, inputString --> unitIdSink),
        div(hidden <-- unitId.map(_.isEmpty).startWith(true),
        div(id := "unit-info", "Stats go here"),
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
        table(cls := "skills-active",
          tr(th("Rarity"), th("Level"), th("Name"),
            th("Description"), th("MP")), children <-- unitActives),
        h3("Traits"),
        table(cls := "skills-trait",
          tr(th("Rarity"), th("Level"), th("Name"),
            th("Description")), children <-- unitTraits),
        div(hidden <-- equippedSkills.map(_.isEmpty),
          h3("Equipped"),
          table(cls := "skills-equip",
            tr(th("Name"),
              th("Description")), children <-- equippedSkills),
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
