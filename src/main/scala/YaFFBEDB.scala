package com.ffbecalc

import scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom.document
import org.scalajs.dom.window

import outwatch.dom._
import rxscalajs.{Observable,Subject}
import rxscalajs.subjects.{BehaviorSubject,ReplaySubject}
import boopickle.Default._
import scala.concurrent.duration.{span => _, _}

object YaFFBEDB {

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
    enhs:  Map[Int,Int],
    esper: Option[Int],
    esperRarity: Int
  ) {
    override def toString = {
      val ue = unit.map { u =>
        esper.fold(u.toString)(esp => u.toString + "," + esp + "," + esperRarity)
      }

      val mats =
        List(mat1, mat2, mat3, mat4).map(_.fold("")(_.toString)).mkString(",")
      val eqs =
        List(rhand, lhand, head, body, acc1, acc2).map(_.fold("")(_.toString)).mkString(",")
      val enh = enhs.toList.map(kv => kv._1 + "=" + kv._2).mkString(",")

      ue.fold("")(_ => List(ue.getOrElse(""), mats, eqs, enh).mkString("/"))
    }
  }
  object PageState {
    import util.Try
    def strId(s: Option[String]): Option[Int] =
      s.flatMap(x => Try(x.toInt).toOption)
    def or0(s: Option[String]): Int = strId(s).getOrElse(0)
    def idOfEq(e: (Option[EquipIndex],Double)) = e._1.map(_.id)
    def idOfMat(e: (Option[MateriaIndex],Double)) = e._1.map(_.id)
    def empty = PageState(None,
      None, None, None, None, None, None,
      None, None, None, None,
      Pots.none, Map.empty, None, 1)
    def from(s: String): PageState = {
      val parts = s.split("/").map(_.split(",")).zipWithIndex
      parts.foldLeft(PageState.empty) { case (ac,(ps,i)) =>
        if (i == 0) {
          val u = strId(ps.headOption)
          val e = strId(ps.drop(1).headOption)
          val r = strId(ps.drop(2).headOption)
          ac.copy(unit = u, esper = e, esperRarity = r.getOrElse(1))
        } else if (i == 1) {
          ac.copy(mat1 = strId(ps.headOption),
            mat2 = strId(ps.drop(1).headOption),
            mat3 = strId(ps.drop(2).headOption),
            mat4 = strId(ps.drop(3).headOption))
        } else if (i == 2) {
          ac.copy(rhand = strId(ps.headOption),
            lhand = strId(ps.drop(1).headOption),
            head = strId(ps.drop(2).headOption),
            body = strId(ps.drop(3).headOption),
            acc1 = strId(ps.drop(4).headOption),
            acc2 = strId(ps.drop(5).headOption))
        } else if (i == 3) {
          val es = ps.map(_.split("=")).foldLeft(Map.empty[Int,Int]) { case (ac,xs) =>
            if (xs(0) != xs(1)) ac + ((xs(0).toInt,xs(1).toInt)) else ac
          }
          ac.copy(enhs = es)
        } else
          ac
      }
    }
    def from(unitId: Option[Int], stats: Option[BaseStats],
      eqs: Equipped, abis: Abilities, enhs: Map[Int,Int],
      esper: Option[Int], esperR: Int) = PageState(
      unitId,
      idOfEq(eqs.rhand),      idOfEq(eqs.lhand),
      idOfEq(eqs.head),       idOfEq(eqs.body),
      idOfEq(eqs.acc1),       idOfEq(eqs.acc2),
      idOfMat(abis.ability1), idOfMat(abis.ability2),
      idOfMat(abis.ability3), idOfMat(abis.ability4),
      stats.fold(Pots.none)(_.pots), enhs,
      esper, esperR)
  }
  @JSExportTopLevel("ffbecalc.main")
  def main(args: Array[String]): Unit = {
    val unitIdSubject = BehaviorSubject[Option[String]](None)
    val unitIdSink = createIdHandler(None)
    val unitId = unitIdSubject.merge(unitIdSink).distinctUntilChanged

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
      })
    val skillRequests = Subject[Int]()
    import collection.immutable.ListMap
    val skillCache = BehaviorSubject[ListMap[Int,Observable[SkillInfo]]](ListMap.empty)
    skillRequests.combineLatest(skillCache.distinctUntilChanged) { case (s,m) =>
      val n = (m + ((s, m.getOrElse(s, Data.get[SkillInfo](s"pickle/skill/$s.pickle"))))).take(100)
      if (n != m) skillCache.next(n)
    }
    def skillInfo(id: Int): Observable[SkillInfo] = {
      skillCache.first.flatMap {
        _.get(id).getOrElse {
          skillRequests.next(id)
          skillCache.dropWhile((x,_) => !x.contains(id)).first.flatMap(_(id))
        }
      }
    }
    val enhancedSkills: Observable[Map[Int,SkillInfo]] = enhancements.flatMap { es =>
      Observable.combineLatest(es.toList.map { case (k,v) =>
        skillInfo(v.newSkill).map(d => (v.oldSkill,d))
      }).map(_.toMap)
    }.publishReplay(1).refCount

    val unitEntry: Observable[Option[UnitEntry]] = unitInfo.map {
      _.fold(Option.empty[UnitEntry])(
        _.entries.values.toList.sortBy(_.rarity).lastOption)
    }.publishReplay(1).refCount

    val limitBurst: Observable[Option[LimitBurst]] = unitInfo.combineLatest(unitEntry).flatMap { case (u,e) =>
      (for {
        unit  <- u
        entry <- e
        k     <- unit.entries.toList.find { case (k,v) => v == entry }.map(_._1)
      } yield {
        Data.get[LimitBurst](s"pickle/lb/$k.pickle").map(Some.apply)
      }).getOrElse(Observable.just(None))
    }

    case class UnitSkills(unit: UnitData, skills: Seq[(UnitSkill, SkillInfo)])
    val unitSkills: Observable[Option[UnitSkills]] = unitInfo.flatMap { u =>
      Observable.combineLatest(u.fold(
        List.empty[Observable[(UnitSkill, SkillInfo)]])(_.skills.map { s =>
        skillInfo(s.id).map { s -> _ }
      })).map(xs => u.map(UnitSkills(_, xs)))
    }

    val esperIdSubject = BehaviorSubject[Option[String]](None)
    val esperRaritySubject = ReplaySubject.withSize[Int](1)
    val esperStats = createHandler[Option[EsperStatInfo]](None)
    val esperSkills = createHandler[List[SkillInfo]](Nil)
    val esper = createHandler[Option[EsperData]](None)
    val esperEntry = createHandler[Option[EsperEntry]](None)

    val unitEffectiveStats = createHandler[Option[UnitStats]](None)
    val battleStats = createHandler[Option[BattleStats]](None)
    val cacheBattleStats = BehaviorSubject[Option[BattleStats]](None)
    battleStats { s => cacheBattleStats.next(s) }

    def equipFor(idOb: Observable[Option[String]]): Observable[Option[EquipIndex]] = for {
      ms <- equips
      id <- idOb
    } yield {
      ms.find(_.id == id.flatMap(i => util.Try(i.toInt).toOption).getOrElse(0))
    }
    val rhandId = createIdHandler(None)
    val rhandSubject = ReplaySubject.withSize[Option[String]](1)
    val rhand = equipFor(rhandId.merge(rhandSubject).distinctUntilChanged)
    val lhandId = createIdHandler(None)
    val lhandSubject = ReplaySubject.withSize[Option[String]](1)
    val lhand = equipFor(lhandId.merge(lhandSubject).distinctUntilChanged)
    val headId = createIdHandler(None)
    val headSubject = ReplaySubject.withSize[Option[String]](1)
    val headEquip = equipFor(headId.merge(headSubject).distinctUntilChanged)
    val bodyId = createIdHandler(None)
    val bodySubject = ReplaySubject.withSize[Option[String]](1)
    val bodyEquip = equipFor(bodyId.merge(bodySubject).distinctUntilChanged)
    val acc1Id = createIdHandler(None)
    val acc1Subject = ReplaySubject.withSize[Option[String]](1)
    val acc1 = equipFor(acc1Id.merge(acc1Subject).distinctUntilChanged)
    val acc2Id = createIdHandler(None)
    val acc2Subject = ReplaySubject.withSize[Option[String]](1)
    val acc2 = equipFor(acc2Id.merge(acc2Subject).distinctUntilChanged)

    val equippedGear = withStamp(rhand).combineLatest(
      withStamp(lhand), withStamp(headEquip), withStamp(bodyEquip))
    val accs = withStamp(acc1).combineLatest(withStamp(acc2))

    val unitStats = createHandler[Option[BaseStats]](None)
    val selectedTraits = createHandler[List[SkillInfo]]()
    val unitPassives = selectedTraits.map(_.flatMap(_.passives)).publishReplay(1).refCount
    val _sorting = createHandler[Sort](Sort.AZ)
    val sorting = _sorting.publishReplay(1).refCount
    val abilitySubjects = AbilitySubjects()
    val abilityValidatorSubjects = AbilitySubjects()
    val (ability1, ability2, ability3, ability4, abilitySlots) =
      components.abilitySlots(materia, unitInfo, unitPassives, unitEntry, sorting, abilitySubjects, abilityValidatorSubjects)
    val abilities = withStamp(ability1).combineLatest(
      withStamp(ability2), withStamp(ability3),
      withStamp(ability4)).map(Abilities.tupled.apply)

    abilities { ab =>
      ab.validateUnique(abilitySubjects, abilityValidatorSubjects)
    }

    val equipped = equippedGear.combineLatest(accs).map { a =>
      Equipped.tupled.apply(a._1 + a._2)
    }.combineLatest(abilities).distinctUntilChanged

    def passivesFrom(equip: List[SkillIndex]) =
      skillsFrom(equip).flatMap(_.passives)

    def skillsFrom(equip: List[SkillIndex]): List[IndexSkillInfo] =
      equip.flatMap(_.skillInfo).foldLeft(Set.empty[Int] -> List.empty[IndexSkillInfo]) { case ((us, xs),x) =>
        val uniqs = if (x.unique) us + x.id else us

        val list = if (us(x.id)) xs else x :: xs

        uniqs -> list
    }._2

    def is2h(eqItem: Option[EquipIndex]): Boolean =
      eqItem.exists(_.twohands)
    def typeOf(eqItem: Option[EquipIndex]): Int =
      eqItem.fold(-1)(_.tpe)
    def isSlot(slot: Int, eqItem: Option[EquipIndex]): Boolean =
      eqItem.exists(_.slotId == slot)

    val allPassives = unitInfo.combineLatest(unitPassives, equipped, esperSkills).map {
      case (info, passives,(eqs,abis), fromEsper) =>
      info -> SkillEffect.collateEffects(info, passivesFrom(eqs.allEquipped ++ abis.allEquipped) ++ passives ++ fromEsper.flatMap(_.passives))
    }

    val equipSkills: Observable[List[IndexSkillInfo]] = equipped.combineLatest(esperSkills).map {
      case ((eqs, abis), fromE) =>
      skillsFrom(eqs.allEquipped ++ abis.allEquipped) ++
        fromE.map(_.asIndexSkillInfo)
    }

    def publishTo[A](sink: Subject[A], value: A): Unit = sink.next(value)

    def passiveOf(e: Option[EquipIndex]): List[SkillEffect] = for {
      equip  <- e.toList
      skills <- equip.skillInfo
      pasv   <- skills.passives
    } yield pasv

    def handValidator(
      r: Option[EquipIndex], l: Option[EquipIndex],
      info: Option[UnitData], effs: SkillEffect.CollatedEffect,
      sink: Subject[Option[String]], older: Boolean): String = {
      if (isSlot(2, r) && isSlot(2, l) && older) { // no DW shields
        publishTo(sink, None)
        EMPTY
      } else if ((is2h(r) || is2h(l)) && older) { // only 1 2h weapon
        publishTo(sink, None)
        EMPTY
      } else if (r.nonEmpty && info.nonEmpty && !effs.canEquip(typeOf(r), info)) {
        publishTo(sink, None)
        EMPTY
      } else if ((isSlot(1, r) && isSlot(1, l)) && (info.nonEmpty && !effs.isEmpty &&
        (!effs.canDualWield(typeOf(r)) || !effs.canDualWield(typeOf(l))) && older)) {

        val eff2 = SkillEffect.collateEffects(info, passiveOf(r) ++ passiveOf(l))
        if (!eff2.canDualWield(typeOf(r)) || !eff2.canDualWield(typeOf(l))) {
          publishTo(sink, None)
          EMPTY
        } else r.fold(EMPTY)(_.id.toString)
      }
      else r.fold(EMPTY)(_.id.toString)
    }

    def equipValidator(
      e: Option[EquipIndex],
      info: Option[UnitData],
      effs: SkillEffect.CollatedEffect, sink: Subject[Option[String]]): String = {
      if (e.nonEmpty && info.nonEmpty && !effs.canEquip(typeOf(e), info)) {
        publishTo(sink, None)
        EMPTY
      } else e.fold(EMPTY)(_.id.toString)
    }

    val rhandValidator = allPassives.combineLatest(equipped).distinctUntilChanged.map {
      case (((info,effs),(eqs, abis))) =>
        handValidator(eqs.rhand._1, eqs.lhand._1, info, effs, rhandSubject, eqs.rhand._2 < eqs.lhand._2)
    }
    val lhandValidator = allPassives.combineLatest(equipped).distinctUntilChanged.map {
      case (((info,effs),(eqs, abis))) =>
        handValidator(eqs.lhand._1, eqs.rhand._1, info, effs, lhandSubject, eqs.lhand._2 < eqs.rhand._2)
    }
    def equipsValidator(sink: Subject[Option[String]], f: Equipped => EqStamp) = allPassives.combineLatest(equipped).distinctUntilChanged.map {
      case (((info,effs),(eqs, abis))) =>
        equipValidator(f(eqs)._1, info, effs, sink)
    }

    val enhSink = createHandler[(Int,Int)]()
    val enhSubject = Subject[(Int,Int)]()
    val enhMap = enhSink.merge(enhSubject).scan(Map.empty[Int,Int]) { (ac, e) =>
      ac + e
    }.distinctUntilChanged.startWith(Map.empty).publishReplay(1).refCount
    def hasEnh(base: SkillInfo, target: SkillInfo) =
      selected <-- enhMap.map { es =>
        es.getOrElse(base.id, base.id) == target.id
      }

    def deco[A,B,C](f: (A,B,C) => VNode): ((A,B,C)) => VNode = f.tupled(_)
    case class UnitActive(unitSkill: UnitSkill, info: SkillInfo, enhs: Map[Int,SkillInfo])
    case class UnitActives(unit: UnitData, actives: Seq[UnitActive])
    val unitActives: Observable[Option[UnitActives]] = unitSkills.combineLatest(enhancedSkills).map { case (us, es) =>
      us.map { x =>
        UnitActives(x.unit, x.skills.filter(_._2.active).map(b => UnitActive(b._1, b._2, es)))
      }
    }
    def enhChain(id: Int, enhs: Map[Int,SkillInfo]): List[Int] = {
      id :: enhs.get(id).fold(List.empty[Int])(x => enhChain(x.id, enhs))
    }
    def relationsOfActives(as: List[ActiveEffect], seed: Map[Int,SkillInfo], seen: Set[Int]): Observable[Seq[SkillInfo]] = {
      val relations = as.flatMap {
        case r: RelatedSkill => r.related
        case _ => Nil
      }

      Observable.combineLatest(relations.map(rid =>
        skillInfo(rid).flatMap { os =>
          if (seen(os.id)) Observable.just(Nil) else
            relationsOfActives(os.actives, Map.empty, seen + os.id).flatMap { rs =>
              Observable.just(List(os) ++ rs ++ seed.values)
            }
        })).scan(Seq.empty[SkillInfo])((xs, x) => x.flatten ++ xs)
    }
    def relationsOf(s: SkillInfo, seed: Map[Int,SkillInfo],
      seen: Set[Int] = Set.empty): Observable[Seq[SkillInfo]] = {
      relationsOfActives(s.actives, seed, seen)
    }
    val describedRelated: Observable[Seq[Int]] =
      unitActives.combineLatest(enhMap).map { case (as, enhm) =>
        as.toList.flatMap(_.actives).flatMap { case UnitActive(_, skill, enhs) =>
          val s = enhancedInfo(skill, enhm.get(skill.id), enhs, identity)

          s.actives.flatMap {
            case r@RandomMagicEffect(_) => r.related
            case r@RandomActiveEffect(_, _, _) => r.related
            case ConditionalSkillEffect(_, ift, iff) => List(ift, iff)
            case _ => Nil
          }
        }.distinct
      }
    val relatedActives: Observable[Map[Int,SkillInfo]] =
      unitInfo.combineLatest(unitActives, enhMap, limitBurst).flatMap { case (u, as, enhm, lb) =>
        if (u == as.map(_.unit)) {
          val seed = as.toList.flatMap(_.actives).map(x => x.info.id -> x.info).toMap
          Observable.combineLatest(lb.toList.map(l => relationsOfActives(l.min.actives, seed, Set.empty)) ++
            as.toList.flatMap(_.actives).map { case UnitActive(_, skill, enhs) =>
              val s = enhancedInfo(skill, enhm.get(skill.id), enhs, identity)

              relationsOf(s, seed)
            }).scan((u,Map.empty[Int,SkillInfo])) { case ((i,xs), x) =>
              if (u == i) (u,x.flatten.map(s => s.id -> s).toMap ++ xs)
              else (u,Map.empty)
            }.map(_._2)
        } else {
          Observable.just(Map.empty[Int,SkillInfo])
        }
      }
    // TODO figure out how to filter out semi-related skills like the various
    // chaos wave abilities
    val filteredRelated =
      relatedActives.combineLatest(unitActives, describedRelated).map { case (ss,us,ds) =>
        (us.toList.flatMap(_.actives).map(_.info.id) ++ ds).foldLeft(ss) {
          (ac, s) => ac - s }
      }

    def renderRelations(info: SkillInfo, e: Map[Int,Int], enhs: Map[Int,SkillInfo], rs: Map[Int,SkillInfo]) = {
      def sk[A](id: Int, empty: A, f: SkillInfo => A) =
        rs.get(id).map(f).getOrElse(empty)
      val s = enhancedInfo(info, e.get(info.id), enhs, identity)
      if (s.actives.exists(_.isInstanceOf[RelatedSkill])) {
        s.actives.zip(s.effects).flatMap {
          case (r: RelatedSkill, _) =>
          if (rs.isEmpty) List(div("Loading"))
          else
          r match {
            case UnlockSkillEffect(skill, turns) =>
              List(p(s"Enable the following skills for ${ActiveUtils.turns(turns)}:", div(sk(skill, "???", _.name))))
            case RandomActiveEffect(skills, tgt, data) =>
              List(div(span("Randomly use:") ::
                skills.map(s => p(
                  s"""${s._2}% ${sk(s._1, "???", _.name)} \u27a1 ${sk(s._1, "???", _.effects.mkString(", "))}"""
                )):_*))
            case RandomMagicEffect(skills) =>
              List(div(span("Randomly cast:") ::
                skills.map(s => p(
                  s"""${s._2}% ${sk(s._1, "???", _.name)} \u27a1 ${sk(s._1, "???", _.effects.mkString(", "))}"""
                )):_*))
            case ConditionalSkillEffect(triggers, ifTrue, ifFalse) =>
              val ts = ActiveUtils.or(triggers.flatMap(rs.get).map(_.name).distinct)
              List(
               p(sk(ifFalse, List.empty[VNode], _.effects.map(div(_))):_*),
               p(span(s"If used after $ts:"), p(sk(ifTrue, List.empty[VNode], x => (if (x.name == s.name) Nil else List(div(b(x.name)))) ++ x.effects.map(div(_))):_*)))

            case MultiAbilityEffect(skills, count) =>
              List((p(s"Use any of the following abilities $count times in one turn:") :: skills.map(s => sk(s, "??? " + s, _.name)).distinct.map(div(_)) :_*))
            case UnlockSkillCountedEffect(skills, turns, uses, target) =>
              val ts = if (turns > 90000) ""
              else s" for ${ActiveUtils.turns(turns)}"
              val uc = if (uses == 1) " (1 time)"
              else if (uses < 100) s" ($uses times)"
              else ""
              p(s"Enable the following skills on $target$ts$uc:") :: skills.map(s => p(sk(s, "???", _.name)))
            case UnlockMultiSkillEffect(skills, count, turns) =>
              p(s"Enable using the following skills $count times each turn for ${ActiveUtils.turns(turns)}:") :: skills.map(s => sk(s, "???", _.name)).distinct.map(s => div(s))
            case _ => List(div("?????"))
          }
          case (_,eff) => List(div(eff))
        }
      } else s.effects.map(div(_))
    }

    def describeActive(rowid: Int, skill: Either[List[ActiveEffect],SkillInfo], content: List[VNode], enhs: Map[Int,SkillInfo] = Map.empty, cols: Int = 5, idn: String = "active"): VNode = {
      // TODO need to query enhs, e.g. doesn't work for hero's rime+2
      val effects = skill.fold(identity, _.actives)
      val hasActiveData = effects.exists(_.isInstanceOf[HasActiveData])
      val hasHealing = effects.exists(_.isInstanceOf[Healing])
      val isExpandable = hasActiveData || hasHealing
      val clickSink = createHandler[Unit]()
      val scanned = clickSink.scan(false) { (b, _) => !b && isExpandable }
      scanned.combineLatest(enhMap, cacheBattleStats) { case (b,enhm,stats) =>
        if (b) {
          val newid = idn + rowid + "-view"
          val exists = document.getElementById(newid)
          if (exists != null)
            exists.parentNode.removeChild(exists)

          val node = document.getElementById(idn + rowid)
          if (node.parentNode != null && node.parentNode.parentNode != null) {
            val newdiv = document.createElement("tr")
            newdiv.id = newid
            newdiv.setAttribute("class", "active-view")
            val next = node.parentNode.parentNode.nextSibling
            if (next == null) {
              node.parentNode.parentNode.parentNode.appendChild(newdiv)
            } else {
              node.parentNode.parentNode.parentNode.insertBefore(newdiv, next)
            }
          }
          val nodes = if (hasActiveData)
            components.renderActiveAttack(skill, enhs, enhm, stats)
          else if (hasHealing)
            components.renderHealing(skill, enhs, enhm, stats)
          else
            Nil
          OutWatch.render("#" + idn + rowid + "-view",
            td((colspan := cols) :: nodes: _*)
          )
        } else {
          val node = document.getElementById(idn + rowid + "-view")
          if (node != null) node.parentNode.removeChild(node)
        }
      }
      if (isExpandable)
        div((cls <-- scanned.startWith(false).map(b => if (b) "active-view-open" else "active-view-close")) :: (id := idn + rowid) :: (click(()) --> clickSink) :: content:_*)
      else
        div(content:_*)
    }
    val activesTable = {
      unitActives.combineLatest(relatedActives).map { case (ss, rs) =>
        val eles = document.querySelectorAll(".active-view")
        (0 until eles.length).foreach(i => eles(i).parentNode.removeChild(eles(i)))
        components.dataTable(ss.toList.flatMap(_.actives).map(UnitActive.unapply(_).get),
          "skills-active",
          List("Rarity", "Level", "Name", "Description", "MP"),
          List("unit-skill-rarity", "unit-skill-level",
            "unit-skill-name", "unit-skill-desc", "unit-skill-cost"))(
          List(
            a => {
              val rowid = a._1.id
              val c = List(img(src := s"https://exviusdb.com/static/img/assets/ability/${a._2.icon}"), span(s"${a._1.rarity}\u2605"))
              describeActive(rowid, Right(a._2), c, a._3)
            },
            a => span(a._1.level.toString),
            deco { (us, info, enhs) =>
              enhancementsOf(info.id, enhs).fold {
                span(info.name)
              } { enh =>
                select(inputString(i => info.id -> i.toInt) --> enhSink,
                  option(value := info.id, info.name, hasEnh(info, info)),
                  option(value := enh._1.id, "+1 " + info.name, hasEnh(info, enh._1)),
                  option(value := enh._2.id, "+2 " + info.name, hasEnh(info, enh._2))
                )
              }
            },
            deco { (us, info, enhs) =>
              div(children <-- enhMap.map { e =>
                val s = enhancedInfo(info, e.get(info.id), enhs, identity)
                renderRelations(s, e, enhs, rs)
              })
            },
            deco { (us, info, enhs) =>
              span(child <-- enhMap.map(e => enhancedInfo(info, e.get(info.id), enhs, _.mpCost.toString)))
            }
          )
        )
      }
    }

    val relatedTable = {

      filteredRelated.combineLatest(relatedActives).map { case (ss,rs) =>
        components.dataTable(ss.values.toList.sortBy(_.id),
          "skills-active",
          List("", "Name", "Effects", "MP"),
          List("unit-skill-rarity", "unit-skill-name",
            "unit-skill-desc", "unit-skill-cost"))(
          List(
            a => div(img(src := s"https://exviusdb.com/static/img/assets/ability/${a.icon}")),
            a => div(s"${a.name} (${a.id})"),
            a =>
              div(children <-- enhMap.map { e =>
                renderRelations(a, e, Map.empty, ss ++ rs)
              }),
            a => div(a.mpCost.toString)
          )
        )
      }
    }

    val traitsTable = {
      // a double-subscription occurs below (???) --> workaround  :-(
      var subscription = Option.empty[rxscalajs.subscription.AnonymousSubscription]

      unitSkills.combineLatest(enhancedSkills).map(a =>
        a._1.toList.flatMap(_.skills).filterNot(_._2.active).map{b =>
        (b._1, b._2, a._2)}).map { ss =>
        val infos = ss.map(_._2).toList
        val enhs = ss.headOption.fold(Map.empty[Int,SkillInfo])(_._3)

        subscription.foreach(_.unsubscribe())
        subscription = Some(selectedTraits <-- enhMap.map { es =>
          infos.flatMap { i =>
            val rid = es.getOrElse(i.id, i.id)
            if (rid == i.id || enhs.isEmpty) List(i)
            else enhs.get(i.id).flatMap { s =>
              if (s.id == rid) Some(s) else enhs.get(s.id)
            }.orElse(enhs.get(rid)).toList
          }
        })
        components.dataTable(ss,
          "skills-trait",
          List("Rarity", "Level", "Name", "Description"),
          List("unit-trait-rarity", "unit-trait-level",
            "unit-trait-name", "unit-trait-desc"))(List(
            a => div(img(src := s"https://exviusdb.com/static/img/assets/ability/${a._2.icon}"), span(s"${a._1.rarity}\u2605")),
            a => span(a._1.level.toString),
            deco { (us, info, enhs) =>
              enhancementsOf(info.id, enhs).fold {
                span(info.name)
              } { enh =>
                select(inputString(i => info.id -> i.toInt) --> enhSink,
                  option(value := info.id, info.name, hasEnh(info, info)),
                  option(value := enh._1.id, "+1 " + info.name, hasEnh(info, enh._1)),
                  option(value := enh._2.id, "+2 " + info.name, hasEnh(info, enh._2))
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
      List("", "Name", "Description"),
      List("unit-equip-icon", "unit-equip-name", "unit-equip-desc"))(List(
        // can't use describeActive here because IndexSkillInfo doesn't fully
        // load ActiveData into ActiveEffect
        a => img(src := s"https://exviusdb.com/static/img/assets/ability/${a.icon}"),
        a => div(a.name.split("\n").map(e => div(e)):_*),
        a => div(a.effects.map(e => div(e)):_*)
      ))
    )
    val trustTable = unitInfo.flatMap { u => (for {
      i <- u
      t <- i.tmr
    } yield t).fold(Observable.just(Option.empty[VNode])) {
      case EquipTrust(x)   => equips.map(_.find(_.id == x).map { e =>
        components.dataTable(List(e),
          "tmr",
          List("", "Name", "Effects"),
          List("tmr-icon", "tmr-name", "tmr-desc"))(List(
            a => img(src := s"https://exviusdb.com/static/img/assets/item/${a.icon}"),
            a => div(a.name),
            a => {
              val is2h = if (e.twohands) "2h" else ""
              div(div(s"$is2h ${SkillEffect.EQUIP(a.tpe)}: ${a.stats} ${a.describeEffects(None)}") :: a.skillInfo.map(s =>
                div(s"${s.name} \u27a1 ${s.effects.mkString(", ")}")): _*)
            }
          ))
      })
      case MateriaTrust(x) => materia.map(_.find(_.id == x).map { m =>
        components.dataTable(List(m),
          "tmr",
          List("", "Name", "Effects"),
          List("tmr-icon", "tmr-name", "tmr-desc"))(List(
            a => img(src := s"https://exviusdb.com/static/img/assets/ability/${a.icon}"),
            a => div(a.name),
            a => {
              div(a.skillInfo.map(s =>
                div(s"${s.name} \u27a1 ${s.effects.mkString(", ")}")): _*)
            }
          ))
      })
    }}.map(_.getOrElse(div("No TMR info available")))

    val limitBurstTable = limitBurst.map { lb =>
      lb.fold(div("Nil")) { limit =>
        components.dataTable(List(limit.min, limit.max),
          "lb",
          List("Level", "Effects", "Cost"),
          List("lb-lvl", "lb-effects", "lb-cost"))(List(
            a => describeActive(limit.name.hashCode + a.hashCode, Left(a.actives), List(div("\u00a0" + (if (limit.min == a) 1 else limit.levels).toString)), cols = 3, idn = "lb-"),
            a => div(a.effects.map(div(_)):_*),
            a => div(a.cost.toString),
          ))
      }
    }
    val unitDescription = unitInfo.map { i =>
      i.fold("")(_.entries.values.toList.sortBy(
        _.rarity).lastOption.fold("Unknown")(_.strings.description.headOption.flatten.getOrElse("Unknown")))
    }

    def effectiveStats(u: UnitData, base: BaseStats, equip: EquipIndex, pasv: SkillEffect.CollatedEffect): Stats = {
      val eqs = equip.stats
      val elements = eqs.element.map(e =>
        SkillEffect.ELEMENTS.getOrElse(e, -1))
      val elestats = elements.map(e =>
        pasv.weapEleStats.getOrElse(e, PassiveStatEffect.zero))
      val eqstats = pasv.equipStats.getOrElse(equip.tpe, PassiveStatEffect.zero)
      val s = Stats.fromEquipStats(equip.stats)
      val innates = SkillEffect.collateEffects(Some(u), equip.skillInfo.flatMap(_.passives))

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
      ((es, (u, passives), sort, base), w) <- equips.combineLatest(allPassives, sorting, unitStats).combineLatest(worn).distinctUntilChanged
    } yield {
      val eqs = es.filter(e =>
        slots(e.slotId) && e.canEquip(u) && passives.canEquip(e.tpe, u))

      List(option(value := EMPTY, "Empty")) ++
        sortFor(eqs, sort, passives, u, base).map { e =>
          val is2h = if (e.twohands) "(2h)" else ""
          option(value := e.id,
            selected := w.exists(_.id == e.id),
            s"${e.name} \u27a1 $is2h ${e.stats} ${e.describeEffects(u)}")
        }
    }

    def eqslot(name: String, slots: Set[Int], validator: Observable[String], worn: Observable[Option[EquipIndex]], sink: outwatch.Sink[Option[String]]): VNode = {
      td(label(name, select(cls := "equip-slot",
        value <-- validator,
        children <-- equippable(slots, worn),
        inputId --> sink)))
    }

    val esperTraining = createHandler[Map[Int,Boolean]](Map.empty)
    val esperTrainingSubject = BehaviorSubject[Map[Int,Boolean]](Map.empty)
    val esperRaritySink = createStringHandler()
    val esperRarity = esperRaritySubject.map(_.toString).merge(esperRaritySink).startWith("1")

    val pageState: Observable[PageState] = equipped.combineLatest(unitStats,unitInfo).combineLatest(espers, esper).combineLatest(esperRarity, enhMap, enhancedSkills).map {
      case (((((eqs,abis),sts,i),es, e)),rarity, enhm, enhs) =>
        PageState.from(
          i.map(_.id), sts, eqs, abis, enhm.filter { case (k,v) => enhs.contains(k) }, e.map(x => es(x.names.head)),
          util.Try(rarity.toInt).getOrElse(1))
    }.publishReplay(1).refCount

    def subscribeChanges = pageState.bufferTime(1.second).map(_.lastOption) {
      case Some(ps) =>
        if (ps.unit.nonEmpty) {
          val pstr = ps.toString
          if (pstr.isEmpty)
            document.location.hash = ""
          else if (document.location.hash.drop(1) != pstr)
            window.history.pushState(0, ps.unit.fold("ffbecalc")("ffbecalc" + _), "#" + pstr)
        }
      case None =>
    }

    def loadFromHash(): Unit = {
      val ps = PageState.from(document.location.hash.drop(1))

      unitIdSubject.next(ps.unit.map(_.toString))
      esperIdSubject.next(ps.esper.map(_.toString))
      esperRaritySubject.next(ps.esperRarity)

      def s(i: Option[Int]): Option[String] = i.map(_.toString)
      abilitySubjects.a1.next(s(ps.mat1))
      abilitySubjects.a2.next(s(ps.mat2))
      abilitySubjects.a3.next(s(ps.mat3))
      abilitySubjects.a4.next(s(ps.mat4))
      acc1Subject.next(s(ps.acc1))
      acc2Subject.next(s(ps.acc2))
      headSubject.next(s(ps.head))
      bodySubject.next(s(ps.body))
      rhandSubject.next(s(ps.rhand))
      lhandSubject.next(s(ps.lhand))

      ps.enhs.toList.foreach(enhSubject.next)
    }

    def updateCheck(): Observable[String] = {
      import scala.concurrent.ExecutionContext.Implicits.global
      import org.scalajs.dom.ext.Ajax
      // a query string to bypass cdn
      val tag = System.currentTimeMillis / (5 * 60 * 1000)
      Observable.from(Ajax.get(url = s"versionCode?$tag",
        responseType = "text",
        headers = Map("Content-Type" -> "text/plain"))).map(_.responseText.trim)
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

      Observable.just(1).concat(Observable.interval(5.minutes)).flatMap(_ =>
        updateCheck()).dropWhile { (s,_) =>
          BuildInfo.versionCode == s }.take(1) { _ =>
        val reloadClick = createHandler[Unit]()
        reloadClick { _ =>
          document.location.reload(true)
          val node = document.getElementById("new-update")
          node.parentNode.removeChild(node)
        }
        OutWatch.render("#content",
          div(id := "new-update",
            span("A new version of ffbecalc is available"),
            button("REFRESH", click(()) --> reloadClick)))
      }
    }

    val resetClick = createHandler[Unit]()
    resetClick { _ =>
      window.history.pushState(0, "reload", "#")
      document.location.reload()
    }
    OutWatch.render("#unit-calculator",
      div(insert --> onLoad,
        div(id := "unit-info",
          select(children <-- unitIndex, inputId --> unitIdSink),
          div(hidden <-- unitId.map(_.isEmpty),
            components.unitStats(unitInfo, unitEntry, unitStats, equipped, allPassives.map(_._2), esper, esperStats, esperEntry, enhancedSkills, enhMap, unitEffectiveStats), button("Reset", click(()) --> resetClick)
          )
        ),
        div(hidden <-- unitId.map(_.isEmpty),
        p(children <-- unitDescription.combineLatest(unitInfo).map { case (d,i) =>
          val eid = i.flatMap(_.entries.toList.sortBy(_._2.rarity).lastOption.map(_._1))
          val cid = i.flatMap(_.entries.toList.sortBy(_._2.rarity).headOption.map(_._2.compendium)).filter(_ < 8000)
          List(
            p(
              a(href := "https://exvius.gamepedia.com/" +
                i.fold(""){_.name.replace(" ", "_")}, "ExviusWiki") ::
              span(" | ") ::
              a(href := "https://exviusdb.com/gl/units/" +
                i.fold(""){_.name.toLowerCase.replace(" ", "-")} + "/",
                  "ExviusDB") ::
              cid.toList.flatMap(c =>
                List(span(" | "),
                a(href := f"https://www.reddit.com/r/FFBraveExvius/wiki/units/$c%03d", "Reddit"))
              ):_*
            )
          ) ++ eid.toList.map(id =>
            img(src := s"https://exviusdb.com/static/img/assets/unit/unit_ills_$id.png", align := "right")) ++ List(p(d))
        }),
        h3("Base Stats"),
        div(child <-- components.unitBaseStats(unitEntry, unitStats, pots)),
        h3("Battle Stats"),
        div(child <-- components.battleStats(unitStats, unitEffectiveStats, battleStats)),
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
        Esper.esperInfo(esper, esperEntry, esperRaritySink, espers, esperIdSubject, esperRaritySubject, esperStats, esperSkills, esperTraining, esperTrainingSubject),
        h3("Abilities & Spells"),
        div(child <-- activesTable),
        h3("Traits"),
        div(child <-- traitsTable),
        div(hidden <-- filteredRelated.map(_.isEmpty),
          h3("Related Skills"),
          div(child <-- relatedTable)
        ),
        div(hidden <-- equipSkills.map(_.isEmpty),
          h3("Equipped"),
          div(child <-- equippedTable),
        ),
        div(hidden <-- unitInfo.map(_.forall(_.tmr.isEmpty)),
          h3("Trust Mastery Reward"),
          div(child <-- trustTable),
        ),
        div(
          h3("Limit Burst: ", child <-- limitBurst.map(_.fold("N/A")(_.name))),
          div(child <-- limitBurstTable),
        ),
        ),
      )
    )
  }
}
