package com.ffbecalc
import io.circe._

object DataDecoders {
  implicit def decodeList[A : Decoder]: Decoder[List[A]] = c => {
    if (!c.downArray.succeeded || c.value.isNull)
      Right(Nil)
    else c.as[List[A]](Decoder.decodeList)
  }

  def decodeActiveEffectList(c: ACursor): List[ActiveEffect] = {
      val active = c.up.up.downField("active").as[Boolean].getOrElse(false) || c.up.up.up.downField("levels").as[Int].fold(_ => false, _ => true)
      if (active) decodeActiveEffect(Nil, c.downArray).fold(_ => Nil, x => List(x))
      else Nil
  }
  def decodeActiveEffect(restrict: List[Int], c: ACursor): Decoder.Result[ActiveEffect] = {
    for {
      x <- c.first.as[Int]
      y <- c.right.as[Int]
      z <- c.right.right.as[Int]
    } yield {
      val a = c.right.right.right
      val isString = (for {
        y <- a.values
        z <- y.headOption
      } yield z.isString).getOrElse(false)
      ActiveDecoders(x, y, z, c.right.right.right)
    }
  }
  def array(a: ACursor): Stream[ACursor] =
    a #:: array(a.right).takeWhile(_.succeeded)

  implicit val decodeActiveEffects: Decoder[List[ActiveEffect]] = c => {
    val self = c.downArray
    val effects = array(self).map(decodeActiveEffectList).toList.flatten.filterNot(_ == UnknownActiveEffect)
    Right(effects)
  }

  def decodePassiveEffect(restrict: List[Int], c: ACursor): Decoder.Result[SkillEffect] = {
    for {
      x <- c.first.as[Int]
      y <- c.right.as[Int]
      z <- c.right.right.as[Int]
    } yield {
      val a = c.right.right.right
      val isString = (for {
        y <- a.values
        z <- y.headOption
      } yield z.isString).getOrElse(false)
      SkillEffect(restrict, x, y, z,
        if (isString) Nil else a.as[List[Int]].fold(_ => Nil, identity))
    }
  }
  def decodePassiveEffectList(c: ACursor): List[SkillEffect] = {
    val active = c.up.up.downField("active").as[Boolean].getOrElse(false)
    if (active) Nil else {
      val restrict =
        c.up.up.downField("unit_restriction").as[Option[List[Int]]].fold(
          _ => Nil, _.toList.flatten)
      decodePassiveEffect(restrict, c.downArray).fold(_ => Nil, x => List(x))
    }
  }
  implicit val decodeSkillEffects: Decoder[List[SkillEffect]] = c => {
    val self = c.downArray
    val effects = array(self).map(decodePassiveEffectList).toList.flatten
    Right(effects)
  }

  implicit val decodeIndexSkillInfo: Decoder[IndexSkillInfo] =
    Decoder.forProduct7(
      "id", "name", "unique", "icon", "desc", "effects", "effects"
    )(IndexSkillInfo.apply)
  implicit val decodeIndexSkillInfoList: Decoder[List[IndexSkillInfo]] = c => {
    import cats.implicits._
    array(c.downArray).map(
      _.as[IndexSkillInfo]).toList.sequence.left.flatMap(_ => Right(Nil))
  }

  implicit val decodeStatRange: Decoder[StatRange] = c => {
    for {
      min <- c.downArray.first.as[Int]
      max <- c.downArray.first.right.as[Int]
      pot <- c.downArray.first.right.right.as[Int]
    } yield StatRange(min, max, pot)
  }
  implicit val decodeWeaponVariance: Decoder[WeaponVariance] = c => {
    (for {
      min <- c.downArray.as[Double]
      max <- c.downArray.right.as[Double]
    } yield WeaponVariance(min, max)).left.flatMap { e =>
      c.up.downField("type_id").as[Int].map(SkillEffect.VARIANCE)
    }
  }
  implicit val decodeAilmentResists: Decoder[AilmentResist] = c => {
    for {
      poison    <- c.downArray.first.as[Int]
      blind     <- c.downArray.first.right.as[Int]
      sleep     <- c.downArray.first.rightN(2).as[Int]
      silence   <- c.downArray.first.rightN(3).as[Int]
      paralysis <- c.downArray.first.rightN(4).as[Int]
      confuse   <- c.downArray.first.rightN(5).as[Int]
      disease   <- c.downArray.first.rightN(6).as[Int]
      petrify   <- c.downArray.first.rightN(7).as[Int]
    } yield AilmentResist(poison, blind, sleep, silence, paralysis, confuse, disease, petrify)
  }
  implicit val decodeEquipAilments: Decoder[EquipAilments] = Decoder.forProduct8(
    "Poison",
    "Blind",
    "Sleep",
    "Silence",
    "Paralyze",
    "Confusion",
    "Disease",
    "Petrify")(EquipAilments.apply)
  implicit val decodeElementResists: Decoder[ElementResist] = c => {
    for {
      fire    <- c.downArray.first.as[Int]
      ice     <- c.downArray.first.right.as[Int]
      thunder <- c.downArray.first.rightN(2).as[Int]
      water   <- c.downArray.first.rightN(3).as[Int]
      wind    <- c.downArray.first.rightN(4).as[Int]
      earth   <- c.downArray.first.rightN(5).as[Int]
      light   <- c.downArray.first.rightN(6).as[Int]
      dark    <- c.downArray.first.rightN(7).as[Int]
    } yield ElementResist(fire, ice, thunder, water, wind, earth, light, dark)
  }
  implicit val decodeEquipElementResist: Decoder[EquipElementResist] = Decoder.forProduct8(
    "Fire",
    "Ice",
    "Lightning",
    "Water",
    "Wind",
    "Earth",
    "Light",
    "Dark")(EquipElementResist.apply)

  implicit val decodeMagicAffinity: Decoder[MagicAffinity] = c =>{
    for {
      white <- c.downArray.first.as[Int]
      black <- c.downArray.first.right.as[Int]
      green <- c.downArray.first.rightN(2).as[Int]
      blue  <- c.downArray.first.rightN(3).as[Int]
    } yield MagicAffinity(white, black, green, blue)
  }
  implicit val decodeStatInfo: Decoder[StatInfo] = c => {
    for {
      hp   <- c.downField("HP").as[StatRange]
      mp   <- c.downField("MP").as[StatRange]
      atk  <- c.downField("ATK").as[StatRange]
      mag  <- c.downField("MAG").as[StatRange]
      defs <- c.downField("DEF").as[StatRange]
      spr  <- c.downField("SPR").as[StatRange]
    } yield StatInfo(hp, mp, atk, mag, defs, spr)
  }
  // have to use custom decoder because of optional unique field
  implicit val decodeMateriaIndexData: Decoder[MateriaIndexData] = c => {
    for {
      id       <- c.downField("id").as[String]
      icon     <- c.downField("icon").as[String]
      uniq     <- c.downField("unique").as[Boolean].left.flatMap(_ => Right(false))
      rarity   <- c.downField("rarity").as[Int]
      mtpe     <- c.downField("magic_type").as[Option[String]]
      skills   <- c.downField("effects_raw").as[List[IndexSkillInfo]]
    } yield MateriaIndexData(id, icon, uniq, rarity, mtpe, skills)
  }

  implicit val decodeEquipStats: Decoder[EquipStats] = Decoder.forProduct10(
    "HP", "MP", "ATK", "DEF", "MAG", "SPR",
    "element_resist", "status_resist", "status_inflict", "element_inflict"
  )(EquipStats.apply)

  implicit val decodeTMR: Decoder[TMR] = c => {
    for {
      x <- c.downArray.as[String]
      y <- c.downArray.right.as[Int]
    } yield {
      x match {
        case "MATERIA" => MateriaTrust(y)
        case "EQUIP"   => EquipTrust(y)
      }
    }
  }
  implicit val decodeEquipReq: Decoder[EquipReq] = c => {
    for {
      x <- c.downArray.as[String]
      y <- c.downArray.right.as[Int]
    } yield {
      x match {
        case "UNIT_ID" => UnitEquipReq(y)
        case "SEX" => SexEquipReq(y)
      }
    }
  }
  implicit val decodeEquipIndexData: Decoder[EquipIndexData] = c => {
    for {
      id       <- c.downField("id").as[Int]
      tpe      <- c.downField("type_id").as[Int]
      icon     <- c.downField("icon").as[String]
      slot     <- c.downField("slot_id").as[Int]
      is2h     <- c.downField("is_twohanded").as[Boolean]
      variance <- c.downField("dmg_variance").as[WeaponVariance]
        .left.flatMap(_ => Right(SkillEffect.VARIANCE(tpe)))
      accuracy <- c.downField("accuracy").as[Int].left.flatMap(_ => Right(0))
      skills   <- c.downField("skills").as[List[Int]]
      stats    <- c.downField("stats").as[EquipStats]
      reqs     <- c.downField("reqs").as[Option[EquipReq]]
      sinfo    <- c.downField("effects_raw").as[List[IndexSkillInfo]]
    } yield EquipIndexData(
      id, icon, slot, is2h, variance, accuracy, skills, tpe, stats, reqs, sinfo)
  }
  // have to use custom decoder because of optional unique field
  implicit val decodeSkillInfo: Decoder[SkillInfo] = c => {
    for {
      id       <- c.downField("id").as[Int]
      name     <- c.downField("name").as[String]
      unique   <- c.downField("unique").as[Boolean].left.flatMap(_ => Right(false))
      active   <- c.downField("active").as[Boolean]
      tpe      <- c.downField("type").as[String]
      icon     <- c.downField("icon").as[String]
      mtpe     <- c.downField("magic_type").as[Option[String]]
      cost     <- c.downField("mp_cost").as[Int]
      actives  <- c.downField("effects_raw").as[List[ActiveEffect]]
      passives <- c.downField("effects_raw").as[List[SkillEffect]]
      effects  <- c.downField("effects").as[List[String]]
    } yield SkillInfo(
      id, name, unique, active, tpe, icon, mtpe, cost,
      actives, passives, effects)
  }
  implicit val decodeUnitStrings: Decoder[UnitStrings] =
    Decoder.forProduct5(
      "description",
      "summon",
      "evolution",
      "affinity",
      "fusion")(UnitStrings.apply)
  implicit val decodeUnitSkill: Decoder[UnitSkill] =
    Decoder.forProduct4("rarity", "level", "type", "id")(UnitSkill.apply)
  implicit val decodeUnitIndexData: Decoder[UnitIndexData] =
    Decoder.forProduct3("min", "max", "id")(UnitIndexData.apply)

  implicit val decodeUnitEntry: Decoder[UnitEntry] =
    Decoder.forProduct9(
      "compendium_id",
      "rarity",
      "stats",
      "limitburst_id",
      "ability_slots",
      "magic_affinity",
      "element_resist",
      "status_resist",
      "strings")(UnitEntry.apply)
  implicit val decodeUnitData: Decoder[UnitData] =
    Decoder.forProduct8(
      "name", "id", "job", "sex", "TMR", "equip", "entries", "skills")(UnitData.apply)
  implicit val decodeEsperStatRange: Decoder[EsperStatRange] = c => {
    for {
      min <- c.downArray.first.as[Int]
      max <- c.downArray.first.right.as[Int]
    } yield EsperStatRange(min, max)
  }
  implicit val decodeEsperStatInfo: Decoder[EsperStatInfo] =
    Decoder.forProduct6(
      "HP", "MP", "ATK", "DEF", "MAG", "SPR")(EsperStatInfo.apply)
  implicit val decodeEsperEntry: Decoder[EsperEntry] =
    Decoder.forProduct3(
      "stats", "element_resist", "status_resist")(EsperEntry.apply)
  implicit val decodeEsperData: Decoder[EsperData] =
    Decoder.forProduct2("names", "entries")(EsperData.apply)
  implicit val decodeEsperSkill: Decoder[EsperSkill] = c => {
    val x = for {
      field <- c.downArray.as[String]
      value <- c.downArray.right.as[Int]
    } yield {
      field match {
        case "HP"      => EsperStatReward.hp(value)
        case "MP"      => EsperStatReward.mp(value)
        case "ATK"     => EsperStatReward.atk(value)
        case "DEF"     => EsperStatReward.defs(value)
        case "MAG"     => EsperStatReward.mag(value)
        case "SPR"     => EsperStatReward.spr(value)
        case "MAGIC"   => UnknownEsperSkill
        case "ABILITY" => EsperAbilityReward(value)
        case _ => UnknownEsperSkill // WaterRes, FireRes, TODO
      }
    }
    Right(x.fold(_ => UnknownEsperSkill, identity))
  }
  implicit val decodeEsperSlot: Decoder[EsperSlot] =
    Decoder.forProduct2("reward", "cost")(EsperSlot.apply)
  implicit val decodeEnhancementStrings: Decoder[EnhancementStrings] =
    Decoder.forProduct2("names", "description")(EnhancementStrings.apply)

  implicit val decodeLimitBurstEffect: Decoder[LimitBurstEffect] =
    Decoder.forProduct3("cost", "effects", "effects_raw")(LimitBurstEffect.apply)
  implicit val decodeLimitBurst: Decoder[LimitBurst] =
    Decoder.forProduct4("name", "levels", "min_level", "max_level")(LimitBurst.apply)
  implicit val decodeEnhancement: Decoder[Enhancement] =
    Decoder.forProduct3("skill_id_old", "skill_id_new", "strings")(Enhancement.apply)
}
