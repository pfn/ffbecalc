package yaffbedb
import io.circe._

object DataDecoders {
  implicit val decodeSkillEffects: Decoder[List[SkillEffect]] = new Decoder[List[SkillEffect]] {
    def array(a: ACursor): Stream[ACursor] =
      a #:: array(a.right).takeWhile(_.succeeded)
    def apply(c: HCursor): Decoder.Result[List[SkillEffect]] = {
      val self = c.downArray
      val effects = array(self).map(decodeSkillEffect).toList.flatten
      Right(effects)
    }

    def decodeSkillEffect(c: ACursor): List[SkillEffect] = {
      if (c.downField("unit_restriction").succeeded) {
        val restrict = c.downField("unit_restriction").as[Option[List[Int]]].fold(
          e => Nil, _.toList.flatten)
        array(c.downField("effects").downArray).map(a =>
          decodeEffect(restrict, a.downArray)).collect {
            case Right(x) => x
          }.filterNot(
            _ == UnknownSkillEffect).toList
      } else {
        decodeEffect(Nil, c.downArray).fold(_ => Nil, x => List(x))
      }
    }

    def decodeEffect(restrict: List[Int], c: ACursor): Decoder.Result[SkillEffect] = {
      for {
        x <- c.first.as[Int]
        y <- c.right.as[Int]
        z <- c.right.right.as[Int]
      } yield {
        val a = c.right.right.right
        val xs = a.values
        val isString = (for {
          y <- a.values
          z <- y.headOption
        } yield z.isString).getOrElse(false)
        SkillEffect(restrict, x, y, z, 
          if (isString) Nil else a.as[List[Int]].fold(_ => Nil, identity))
      }
    }
  }
  implicit val decodeStatRange: Decoder[StatRange] = new Decoder[StatRange] {
    def apply(c: HCursor): Decoder.Result[StatRange] = {
      for {
        min <- c.downArray.first.as[Int]
        max <- c.downArray.first.right.as[Int]
        pot <- c.downArray.first.right.right.as[Int]
      } yield StatRange(min, max, pot)
    }
  }
  implicit val decodeAilmentResists: Decoder[AilmentResist] = new Decoder[AilmentResist] {
    def apply(c: HCursor): Decoder.Result[AilmentResist] = {
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
  }
  implicit val decodeEquipAilments: Decoder[EquipAilments] = Decoder.forProduct8(
    "Poison",
    "Blind",
    "Sleep",
    "Silence",
    "Paralysis",
    "Confusion",
    "Disease",
    "Petrify")(EquipAilments.apply)
  implicit val decodeElementResists: Decoder[ElementResist] = new Decoder[ElementResist] {
    def apply(c: HCursor): Decoder.Result[ElementResist] = {
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

  implicit val decodeMagicAffinity: Decoder[MagicAffinity] = new Decoder[MagicAffinity] {
    def apply(c: HCursor): Decoder.Result[MagicAffinity] = {
      for {
        white <- c.downArray.first.as[Int]
        black <- c.downArray.first.right.as[Int]
        green <- c.downArray.first.rightN(2).as[Int]
        blue  <- c.downArray.first.rightN(3).as[Int]
      } yield MagicAffinity(white, black, green, blue)
    }
  }
  implicit val decodeStatInfo: Decoder[StatInfo] = new Decoder[StatInfo] {
    def apply(c: HCursor): Decoder.Result[StatInfo] = {
      for {
        hp   <- c.downField("HP").as[StatRange]
        mp   <- c.downField("MP").as[StatRange]
        atk  <- c.downField("ATK").as[StatRange]
        mag  <- c.downField("MAG").as[StatRange]
        defs <- c.downField("DEF").as[StatRange]
        spr  <- c.downField("SPR").as[StatRange]
      } yield StatInfo(hp, mp, atk, mag, defs, spr)
    }
  }
  implicit val decodeMateriaIndexData: Decoder[MateriaIndexData] =
    Decoder.forProduct5("id", "effects", "rarity", "magic_type", "effects_raw")(MateriaIndexData.apply)

  implicit val decodeEquipStats: Decoder[EquipStats] = Decoder.forProduct10(
    "HP", "MP", "ATK", "DEF", "MAG", "SPR",
    "element_resist", "status_resist", "status_inflict", "element_inflict"
  )(EquipStats.apply)

  implicit val decodeEquipReq: Decoder[EquipReq] = new Decoder[EquipReq] {
    def apply(c: HCursor): Decoder.Result[EquipReq] = {
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
  }
  implicit val decodeEquipIndexData: Decoder[EquipIndexData] =
    Decoder.forProduct9(
      "id",
      "slot_id",
      "skills",
      "type_id",
      "effects_raw",
      "effects",
      "skill_effects",
      "stats",
      "reqs"
    )(EquipIndexData.apply)
  implicit val decodeSkillInfo: Decoder[SkillInfo] =
    Decoder.forProduct7(
      "name",
      "active",
      "type",
      "magic_type",
      "mp_cost",
      "effects_raw",
      "effects")(SkillInfo.apply)
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
    Decoder.forProduct8(
      "rarity",
      "stats",
      "limitburst_id",
      "ability_slots",
      "magic_affinity",
      "element_resist",
      "status_resist",
      "strings")(UnitEntry.apply)
  implicit val decodeUnitData: Decoder[UnitData] =
    Decoder.forProduct7(
      "name", "id", "job", "sex", "equip", "entries", "skills")(UnitData.apply)
  implicit val decodeEsperStatRange: Decoder[EsperStatRange] = new Decoder[EsperStatRange] {
    def apply(c: HCursor): Decoder.Result[EsperStatRange] = {
      for {
        min <- c.downArray.first.as[Int]
        max <- c.downArray.first.right.as[Int]
      } yield EsperStatRange(min, max)
    }
  }
  implicit val decodeEsperStatInfo: Decoder[EsperStatInfo] =
    Decoder.forProduct6(
      "HP", "MP", "ATK", "DEF", "MAG", "SPR")(EsperStatInfo.apply)
  implicit val decodeEsperEntry: Decoder[EsperEntry] =
    Decoder.forProduct3(
      "stats", "element_resist", "status_resist")(EsperEntry.apply)
  implicit val decodeEsperData: Decoder[EsperData] =
    Decoder.forProduct2("names", "entries")(EsperData.apply)
  implicit val decodeEsperSkill: Decoder[EsperSkill] = new Decoder[EsperSkill] {
    def apply(c: HCursor): Decoder.Result[EsperSkill] = {
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
        }
      }
      Right(x.fold(_ => UnknownEsperSkill, identity))
    }
  }
  implicit val decodeEsperSlot: Decoder[EsperSlot] =
    Decoder.forProduct2("reward", "cost")(EsperSlot.apply)
}
