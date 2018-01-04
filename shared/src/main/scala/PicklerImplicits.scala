package yaffbedb
import boopickle.Default._

trait PicklerImplicits {
  implicit val activesPickler      = implicitly[Pickler[List[ActiveEffect]]]
  implicit val skillInfoPickler    = implicitly[Pickler[SkillInfo]]
  implicit val unitsPickler        = implicitly[Pickler[List[UnitIndex]]]
  implicit val equipsPickler       = implicitly[Pickler[List[EquipIndex]]]
  implicit val materiaPickler      = implicitly[Pickler[List[MateriaIndex]]]
  implicit val esperSlotsPickler   = implicitly[Pickler[List[EsperSlot]]]
  implicit val esperDataPickler    = implicitly[Pickler[EsperData]]
  implicit val enhancementPickler  = implicitly[Pickler[Enhancement]]
  implicit val enhancementsPickler = implicitly[Pickler[Map[String,Enhancement]]]
  implicit val unitDataPickler     = implicitly[Pickler[UnitData]]
}
