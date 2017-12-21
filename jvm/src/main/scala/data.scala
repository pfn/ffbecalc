package yaffbedb
import io.circe._
import optics.JsonPath.root

object ActiveDataExtract {
  val elements = root.element_inflict.each.string
  val tpe = root.`type`.string
  val atkframes = root.attack_frames.each.arr
  val atkdmg = root.attack_damage.each.arr
  val atkcount = root.attack_count.each.int
  val atktpe = root.attack_type.string
  val movetpe = root.move_type.int
  val motiontpe = root.motion_type.int

  def empty = ActiveData(Nil, "------", Nil, Nil, Nil, "None", 0, 0)
  def listList[T: Decoder](p: monocle.Traversal[Json,
    Vector[Json]], j: Json, default: T): List[List[T]] =
      p.getAll(j).map(_.toList.map(_.as[T].getOrElse(default)))
  def apply(c: ACursor): ActiveData = {
    val top = c.up.up.up.focus
    val t = top.flatMap(tpe.getOption)
    val fs = top.toList.map(atkframes.getAll)
    (for {
      t    <- top
      tp   <- tpe.getOption(t)
      atpe <- atktpe.getOption(t)
      mtpe <- movetpe.getOption(t)
    } yield ActiveData(elements.getAll(t), tp,
      listList(atkframes, t, 0),
      listList(atkdmg, t, 0),
      atkcount.getAll(t), atpe, mtpe,
      motiontpe.getOption(t).getOrElse(0))) getOrElse empty
  }
}

object ActiveDecoders {
  def listInt(c: Option[Json]): List[Int] = c.fold(List.empty[Int])(root.each.int.getAll)

  def apply(x: Int, y: Int, active: Int, c: ACursor) = {
    decoders(active)(SkillTarget(x, y), ActiveDataExtract(c), c.focus)
  }

  val decoders: Map[Int, (SkillTarget,ActiveData,Option[Json]) => ActiveEffect] = Map(
    1 -> {
      (t,a,c) => {
        PhysicalEffect(listInt(c)(6), 0, t, a)
      }
    },
    2 -> {
      (t, a, c) => {
        val xs = listInt(c)
        HealEffect(xs(3), xs(2), 1, t)
      }
    },
    3 -> {
      (t, a, c) => {
        val xs = listInt(c)
        BuffEffect(xs(0), xs(1), xs(2), xs(3), xs(4), t)
      }
    },
    4 -> {
      (t, a, c) => {
        RaiseEffect(listInt(c)(0), t)
      }
    },
    5 -> {
      (t, a, c) => {
        val xs = listInt(c)
        EsunaEffect(xs(0) != 0, xs(1) != 0, xs(2) != 0, xs(3) != 0,
          xs(4) != 0, xs(5) != 0, xs(6) != 0, xs(7) != 0, t)
      }
    },
    6 -> {
      (t, a, c) => {
        val xs = listInt(c)
        StatusAilmentEffect(xs(0), xs(1), xs(2), xs(3),
          xs(4), xs(5), xs(6), xs(7), t)
      }
    },
    7 -> {
      (t, a, c) => {
        val xs = listInt(c)
        AilmentResistEffect(xs(0), xs(1), xs(2), xs(3),
          xs(4), xs(5), xs(6), xs(7), xs(9), t)
      }
    },
    8 -> {
      (t, a, c) => {
        val xs = listInt(c)
        HealEffect(xs(0), xs(2), xs(3), t)
      }
    },
    9 -> {
      (t, a, c) => {
        val xs = listInt(c)
        PercentHPDamageEffect(xs(0), xs(1), t, a)
      }
    },
    10 -> {
      (t, a, c) => {
        val xs = listInt(c)
        MPDrainEffect(xs(1), xs(0), t, a)
      }
    },
    11 -> {
      (t, a, c) => {
        val xs = listInt(c)
        SacrificeSelfRestoreEffect(xs(0), xs(1), t)
      }
    },
    13 -> {
      (t, a, c) => {
        val xs = listInt(c)
        // index 4 may not be an int, skip it with index 5
        val ratio = c.flatMap(root(5).int.getOption).getOrElse(0)
        DelayDamageEffect(ratio, xs(0), t, a)
      }
    },
    15 -> {
      (t, a, c) => {
        val ratio = c.flatMap(root(5).int.getOption).getOrElse(0)
        MagicalEffect(ratio, 0, t, a)
      }
    },
    16 -> {
      (t, a, c) => {
        val hp = c.flatMap(root(0).int.getOption).getOrElse(0)
        RestoreEffect(hp, 0, t)
      }
    },
    17 -> {
      (t, a, c) => {
        val mp = c.flatMap(root(0).int.getOption).getOrElse(0)
        RestoreEffect(0, mp, t)
      }
    },
    18 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ReducePhysicalDamageEffect(xs(0), xs(1), t)
      }
    },
    19 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ReduceMagicalDamageEffect(xs(0), xs(1), t)
      }
    },
    20 -> {
      (t, a, c) => {
        val xs = listInt(c)
        StoreAttackEffect(xs(0), xs(1), xs(2), t)
      }
    },
    21 -> {
      (t, a, c) => {
        val xs = listInt(c)
        PhysicalEffect(xs(2), xs(3), t, a)
      }
    },
    22 -> {
      (t, a, c) => {
        val xs = listInt(c)
        PhysicalKillerEffect(xs(3), xs(0), t, a)
      }
    },
    23 -> {
      (t, a, c) => {
        val xs = listInt(c)
        MagicalKillerEffect(xs(3), xs(0), t, a)
      }
    },
    24 -> {
      (t, a, c) => {
        val xs = listInt(c)
        DebuffEffect(xs(0), xs(1), xs(2), xs(3), xs(4), t)
      }
    },
    25 -> {
      (t, a, c) => {
        val xs = listInt(c)
        HPDrainEffect(xs(1), xs(0), t, a)
      }
    },
    26 -> {
      (t, a, c) => {
        val xs = listInt(c)
        RestorePercentEffect(xs(0), 0, t)
      }
    },
    27 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ReraiseEffect(xs(0), xs(1), t)
      }
    },
    28 -> {
      (t, a, c) => {
        SalveEffect(listInt(c), t)
      }
    },
    29 -> {
      (t, a, c) => {
        val sks = c.toList.flatMap(j => root.each.arr.getAll(j).map(v => v(0).as[Int].getOrElse(0) -> v(1).as[Int].getOrElse(0)))
        RandomActiveEffect(sks, t, a)
      }
    },
    30 -> {
      (t, a, c) => {
        val xs = listInt(c)
        MPHealEffect(xs(0), xs(2), xs(3), t)
      }
    },
    31 -> { (t,a,c) => EntrustEffect(t) },
    32 -> {
      (t, a, c) => {
        val xs = listInt(c)
        EsperFillEffect(xs(0), xs(1), t)
      }
    },
    33 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ElementResistEffect(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(9), t)
      }
    },
    34 -> {
      (t, a, c) => {
        val xs = listInt(c)
        RandomAilmentEffect(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(9), t)
      }
    },
    35 -> {
      (t, a, c) => {
        val chance = c.flatMap(root(0).int.getOption).getOrElse(0)
        InstantKOEffect(chance, t)
      }
    },
    37 -> {
      (t, a, c) => {
        val chance = c.flatMap(root(0).int.getOption).getOrElse(0)
        StealEffect(chance, t)
      }
    },
    38 -> {
      (t, a, c) => {
        val xs = listInt(c)
        HealChanceEffect(xs(4), xs(3), xs(2), t)
      }
    },
    39 -> {
      (t, a, c) => {
        val xs = listInt(c)
        SacrificeHPPercentDamageEffect(xs(1), xs(3), t, a)
      }
    },
    40 -> {
      (t, a, c) => {
        val xs = listInt(c)
        HybridEffect(xs(8), xs(9), t, a)
      }
    },
    41 -> {
      (t, a, c) => {
        val damage = c.flatMap(root(0).int.getOption).getOrElse(0)
        FixedDamageEffect(damage, t, a)
      }
    },
    42 -> {
      (t, a, c) => {
        val xs = listInt(c)
        RepeatAttackEffect(xs(4), xs(2), xs(3), t, a)
      }
    },
    43 -> {
      (t, a, c) => {
        val xs = listInt(c)
        CriticalAttackEffect(xs(2), xs(3), t, a)
      }
    },
    44 -> { (t,a,c) => DualBlackMagicEffect },
    45 -> { (t,a,c) => DualCastEffect },
    47 -> {
      (t, a, c) => {
        val xs = listInt(c)
        if (c.flatMap(root(0).int.getOption).getOrElse(0) != 134)
          sys.error("Libra effect should have argument 134")
        LibraEffect(t)
      }
    },
    48 -> { (t,a,c) => DrinkEffect },
    50 -> { (t,a,c) => ThrowEffect },
    51 -> { (t,a,c) => DualCastEffect },
    52 -> {
      (t, a, c) => {
        val xs = listInt(c)
        if (t == SkillTarget(Target.Self, Target.Self)) {
          if (xs.size > 3) {
            // not quite right, whitecount/greencount can be reversed?
            DualMagicEffect(xs(0) == 2 || xs(1) == 2, xs(0) == 3 || xs(1) == 3, xs(2), xs(3))
          } else {
            if (xs(0) == 2)
              DualMagicEffect(true, false, xs(1), 0)
            else
              DualMagicEffect(false, true, xs(1), 0)
          }
        } else {
          JumpDamageEffect(xs(4), xs(2), t, a)
        }
      }
    },
    53 -> {
      (t, a, c) => {
        val xs = listInt(c)
        HideEffect(xs(0), xs(1), t)
      }
    },
    54 -> {
      (t, a, c) => {
        val xs = listInt(c)
        DodgePhysicalEffect(xs(0), xs(1), t)
      }
    },
    56 -> {
      (t, a, c) => {
        val xs = listInt(c)
        if (t.cls == Target.Self)
          SkipTurnsEffect(xs(3), t)
        else
          SingingHealEffect(xs(0), xs(2), xs(3), t)
      }
    },
    57 -> {
      (t, a, c) => {
        val xs = listInt(c)
        SingingMPHealEffect(xs(0), xs(2), xs(3), t)
      }
    },
    58 -> {
      (t, a, c) => {
        val xs = listInt(c)
        SingingBuffEffect(xs(0), xs(1), xs(2), xs(3), xs(4), t)
      }
    },
    59 -> { (t,a,c) => DispelEffect(t) },
    60 -> {
      (t, a, c) => {
        val xs = listInt(c)
        CharmEffect(xs(0), xs(1), t)
      }
    },
    60 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ProvokeEffect(xs(0), xs(1), t)
      }
    },
    61 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ProvokeEffect(xs(0), xs(1), t)
      }
    },
    63 -> {
      (t, a, c) => {
        val xs = listInt(c)
        LBRateEffect(xs(0), xs(1), t)
      }
    },
    64 -> {
      (t, a, c) => {
        val xs = listInt(c)
        RestorePercentEffect(xs(0), xs(1), t)
      }
    },
    65 -> {
      (t, a, c) => {
        val xs = listInt(c)
        RestoreEffect(xs(0), xs(1), t)
      }
    },
    66 -> {
      (t, a, c) => {
        val xs = listInt(c)
        GiantEffect(xs(0), xs(1), xs(2), t)
      }
    },
    68 -> {
      (t, a, c) => {
        val xs = listInt(c)
        BerserkEffect(xs(0), xs(1), t)
      }
    },
    70 -> {
      (t, a, c) => {
        val xs = listInt(c)
        MagicalEffect(xs(2), xs(3), t, a)
      }
    },
    71 -> { (t,a,c) => InvokeSkillEffect(listInt(c)(0)) },
    72 -> {
      (t, a, c) => {
        val xs = listInt(c)
        StackingMagicalEffect(xs(2), xs(3), xs(5), t, a)
      }
    },
    76 -> {
      (t, a, c) => {
        val xs = listInt(c)
        StealGilEffect(xs(0), xs(1), t)
      }
    },
    76 -> {
      (t, a, c) => {
        val xs = listInt(c)
        StealGilEffect(xs(0), xs(1), t)
      }
    },
    81 -> {
      (t, a, c) => {
        val xs = listInt(c)
        SacrificeHPDamageEffect(xs(6), xs(7), t, a)
      }
    },
    82 -> {
      (t, a, c) => {
        val sks = c.toList.flatMap(j => root.each.arr.getAll(j).map(v => v(0).as[Int].getOrElse(0) -> v(1).as[Int].getOrElse(0)))
        RandomMagicEffect(sks)
      }
    },
    83 -> {
      (t, a, c) => {
        SetHPEffect(listInt(c)(0), t)
      }
    },
    84 -> {
      (t, a, c) => {
        val xs = listInt(c)
        SealingBladeEffect(xs(0), xs(2))
      }
    },
    86 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ReflectEffect(xs(1), xs(2), t)
      }
    },
    88 -> {
      (t, a, c) => {
        val xs = listInt(c)
        StopEffect(xs(0), xs(1), t)
      }
    },
    89 -> {
      (t, a, c) => {
        val xs = listInt(c)
        DebuffResistEffect(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), t)
      }
    },
    90 -> {
      (t, a, c) => {
        val xs = listInt(c)
        MagStoreEffect(xs(0), xs(1))
      }
    },
    92 -> {
      (t, a, c) => {
        val xs = c.toList.flatMap(root(0).each.int.getAll)
        ImbuePKillerEffect(xs(0), xs(1), t)
      }
    },
    93 -> {
      (t, a, c) => {
        val xs = c.toList.flatMap(root(0).each.int.getAll)
        ImbueMKillerEffect(xs(0), xs(1), t)
      }
    },
    95 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ImbueElementEffect(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), xs(6), xs(7), xs(8), t)
      }
    },
    96 -> {
      (t, a, c) => {
        val xs = listInt(c)
        PhysicalAllCoverEffect(xs(4), xs(2), xs(3), xs(6), t)
      }
    },
    97 -> {
      (t, a, c) => {
        val xs = listInt(c)
        UnlockSkillEffect(xs(2), xs(3))
      }
    },
    98 -> {
      (t, a, c) => {
        val xs = listInt(c)
        if (xs.size < 4) {
          val ys = c.toList.flatMap(root(3).each.int.getAll)
          val turns = c.flatMap(root(4).int.getOption).getOrElse(0)
          UnlockMultiSkillEffect(ys, xs(0), turns)
        } else {
          UnlockMultiSkillEffect(xs(3) :: Nil, xs(1), xs(4))
        }
      }
    },
    99 -> {
      (t, a, c) => {
        val xs = listInt(c)
        if (xs.size != 6) {
          val ys = c.toList.flatMap(root(1).each.int.getAll)
          val ifTrue = c.flatMap(root(3).int.getOption).getOrElse(0)
          val ifFalse = c.flatMap(root(5).int.getOption).getOrElse(0)
          ConditionalSkillEffect(ys, ifTrue, ifFalse)
        } else {
          ConditionalSkillEffect(xs(1) :: Nil, xs(3), xs(5))
        }
      }
    },
    100 -> {
      (t, a, c) => {
        val xs = listInt(c)
        if (xs.size == 3) {
          val ys = c.toList.flatMap(root(1).each.int.getAll)
          val uses = c.flatMap(root(2).int.getOption).getOrElse(0)
          val turns = c.flatMap(root(3).int.getOption).getOrElse(0)
          UnlockSkillCountedEffect(ys, turns, uses)
        } else {
          UnlockSkillCountedEffect(xs(1) :: Nil, xs(2), xs(3))
        }
      }
    },
    101 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ReduceDamageEffect(xs(0), xs(1), t)
      }
    },
    103 -> {
      (t, a, c) => {
        val xs = listInt(c)
        SprDamageEffect(xs(2), xs(1), xs(0), t, a)
      }
    },
    105 -> {
      (t, a, c) => {
        val xs = listInt(c)
        MPDamageEffect(xs(2), xs(1), xs(0), t, a)
      }
    },
    111 -> {
      (t, a, c) => {
        val xs = listInt(c)
        DebuffRemoveEffect(xs(0), xs(1), xs(2), xs(3), xs(4), xs(5), t)
      }
    },
    112 -> {
      (t, a, c) => {
        val xs = listInt(c)
        DamageOrDeathEffect(xs(0), xs(2), xs(1), t, a)
      }
    },
    118 -> {
      (t, a, c) => {
        val xs = listInt(c)
        PhysicalTargetCoverEffect(xs(2), xs(0), xs(1), xs(3), t)
      }
    },
    119 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ActiveCounterEffect(xs(0), xs(1), xs(2), xs(3), t)
      }
    },
    122 -> { (t, a, c) => DeathImmunityEffect(listInt(c)(0), t) },
    123 -> {
      (t, a, c) => {
        val xs = listInt(c)
        ActiveCounterEffect(xs(0), xs(1), xs(2), xs(3), t)
      }
    },
    124 -> {
      (t, a, c) => {
        val xs = listInt(c)
        val splits = c.fold(List.empty[Int])(root(9).each.int.getAll)
        if (splits.size != 2) sys.error("Expected 2 splits: " + splits.size)
        EvokeDamageEffect(xs(7), xs(8), splits, t, a)
      }
    },
    125 -> {
      (t, a, c) => {
        val xs = listInt(c)
        LBFillEffect(xs(0), xs(1), t)
      }
    },
    1002 -> { (t, a, c) => SurvivorFlaskEffect },
    1003 -> {
      (t, a, c) => {
        val xs = listInt(c)
        LBFillPercentEffect(xs(1), t)
      }
    },
    1005 -> { (t, a, c) => TwistOfFateEffect },
    1006 -> {
      (t, a, c) => {
        val count = c.fold(0)(root(0).int.getOption(_).getOrElse(0))
        val skills = c.fold(List.empty[Int])(root(1).each.int.getAll)
        MultiAbilityEffect(skills, count)
      }
    },
    1007 -> {
      (t, a, c) => {
        val xs = listInt(c)
        StackingPhysicalEffect(xs(2), xs(3), xs(5), t, a)
      }
    },
    1011 -> { (t, a, c) => SkipTurnsEffect(listInt(c)(0), t) },
    1012 -> { (t, a, c) => HexDebuffAttackEffect(t, a) },
  )
}

object DataDecoders {
  implicit def decodeList[A : Decoder]: Decoder[List[A]] = c => {
    if (!c.downArray.succeeded || c.value.isNull)
      Right(Nil)
    else c.as[List[A]](Decoder.decodeList)
  }
  def array(a: ACursor): Stream[ACursor] =
    a #:: array(a.right).takeWhile(_.succeeded)
  implicit val decodeActiveEffects: Decoder[List[ActiveEffect]] = new Decoder[List[ActiveEffect]] {
    def decodeSkillEffect(c: ACursor): List[ActiveEffect] = {
        val active = c.up.up.downField("active").as[Boolean].getOrElse(false)
        if (active) decodeEffect(Nil, c.downArray).fold(_ => Nil, x => List(x))
        else Nil
    }
    def decodeEffect(restrict: List[Int], c: ACursor): Decoder.Result[ActiveEffect] = {
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
    def apply(c: HCursor): Decoder.Result[List[ActiveEffect]] = {
      val self = c.downArray
      val effects = array(self).map(decodeSkillEffect).toList.flatten.filterNot(_ == UnknownActiveEffect)
      Right(effects)
    }
  }
  implicit val decodeSkillEffects: Decoder[List[SkillEffect]] = new Decoder[List[SkillEffect]] {

    def apply(c: HCursor): Decoder.Result[List[SkillEffect]] = {
      val self = c.downArray
      val effects = array(self).map(decodeSkillEffect).toList.flatten
      Right(effects)
    }

    def decodeSkillEffect(c: ACursor): List[SkillEffect] = {
      if (c.downField("unit_restriction").succeeded) {
        val restrict = c.downField("unit_restriction").as[Option[List[Int]]].fold(
          _ => Nil, _.toList.flatten)
        array(c.downField("effects").downArray).map(a =>
          decodeEffect(restrict, a.downArray)).collect {
            case Right(x) => x
          }.filterNot(
            _ == UnknownSkillEffect).toList
      } else {
        val active = c.up.up.downField("active").as[Boolean].getOrElse(false)
        if (active) Nil else
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
        val isString = (for {
          y <- a.values
          z <- y.headOption
        } yield z.isString).getOrElse(false)
        SkillEffect(restrict, x, y, z, 
          if (isString) Nil else a.as[List[Int]].fold(_ => Nil, identity))
      }
    }
  }
  implicit val decodeStatRange: Decoder[StatRange] = c => {
    for {
      min <- c.downArray.first.as[Int]
      max <- c.downArray.first.right.as[Int]
      pot <- c.downArray.first.right.right.as[Int]
    } yield StatRange(min, max, pot)
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
  implicit val decodeMateriaIndexData: Decoder[MateriaIndexData] =
    Decoder.forProduct5("id", "effects", "rarity", "magic_type", "effects_raw")(MateriaIndexData.apply)

  implicit val decodeEquipStats: Decoder[EquipStats] = Decoder.forProduct10(
    "HP", "MP", "ATK", "DEF", "MAG", "SPR",
    "element_resist", "status_resist", "status_inflict", "element_inflict"
  )(EquipStats.apply)

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
  implicit val decodeEquipIndexData: Decoder[EquipIndexData] =
    Decoder.forProduct9(
      "id",
      "slot_id",
      "is_twohanded",
      "skills",
      "type_id",
      "effects_raw",
      "skill_effects",
      "stats",
      "reqs"
    )(EquipIndexData.apply)
  implicit val decodeSkillInfo: Decoder[SkillInfo] =
    Decoder.forProduct10(
      "id",
      "name",
      "active",
      "type",
      "icon",
      "magic_type",
      "mp_cost",
      "effects_raw",
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

  implicit val decodeEnhancement: Decoder[Enhancement] =
    Decoder.forProduct3("skill_id_old", "skill_id_new", "strings")(Enhancement.apply)
}
