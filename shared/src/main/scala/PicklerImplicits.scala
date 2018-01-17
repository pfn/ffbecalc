package com.ffbecalc
import boopickle.Default._

trait PicklerImplicits {
  implicit val activePickler       = implicitly[Pickler[ActiveEffect]]
  implicit val skillEffectPickler  = implicitly[Pickler[SkillEffect]]
  implicit val activesPickler      = implicitly[Pickler[List[ActiveEffect]]]
  implicit val indexSkillPickler   = implicitly[Pickler[IndexSkillInfo]]
  implicit val skillPickler        = implicitly[Pickler[SkillInfo]]
  implicit val enhancePickler      = implicitly[Pickler[Enhancement]]
  implicit val equipPickler        = implicitly[Pickler[EquipIndex]]
  implicit val unitPickler         = implicitly[Pickler[UnitIndex]]
  implicit val materiaPickler      = implicitly[Pickler[MateriaIndex]]
  implicit val unitDataPickler     = implicitly[Pickler[UnitData]]
  implicit val lbPickler           = implicitly[Pickler[LimitBurst]]
  implicit val esperPickler        = implicitly[Pickler[EsperData]]
  implicit val esperSlotPickler    = implicitly[Pickler[EsperSlot]]

  implicit val unitsPickler        = implicitly[Pickler[List[UnitIndex]]]
  implicit val equipsPickler       = implicitly[Pickler[List[EquipIndex]]]
  implicit val materiasPickler     = implicitly[Pickler[List[MateriaIndex]]]
  implicit val esperSlotsPickler   = implicitly[Pickler[List[EsperSlot]]]
  implicit val enhancementsPickler = implicitly[Pickler[Map[String,Enhancement]]]
}
