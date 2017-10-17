import rxscalajs.{Observable,Subject}
import java.util.UUID

package object yaffbedb {
  // why isn't the definition in outwatch.dom visible to us?
  type Handler[T] = Observable[T] with outwatch.Sink[T]

  type EqStamp = (Option[EquipIndex],Double)
  type MatStamp = (Option[MateriaIndex],Double)
  def withStamp[A](ob: Observable[A]): Observable[(A,Double)] =
    ob.map(_ -> scalajs.js.Date.now())

  val EMPTY = "--empty--"
  val forId = outwatch.dom.forLabel
  def uuid() = java.util.UUID.randomUUID().toString
  implicit class Tuple4Plus[A,B,C,D](val tuple: (A,B,C,D)) extends AnyVal {
    def +[E,F](other: (E,F)): (A,B,C,D,E,F) =
      (tuple._1, tuple._2, tuple._3, tuple._4, other._1, other._2)
    def +[E](other: E): (A,B,C,D,E) =
      (tuple._1, tuple._2, tuple._3, tuple._4, other)
  }

  val inputId = outwatch.dom.inputString(maybeId)
  def createIdHandler(ss: Option[String]*) = outwatch.dom.createHandler[Option[String]](ss:_*)

  def maybeId(id: String): Option[String] =
    if (id == EMPTY) None else Some(id)
}

package yaffbedb {
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

case class PotSubjects(hp: Subject[Int], mp: Subject[Int], atk: Subject[Int], defs: Subject[Int], mag: Subject[Int], spr: Subject[Int]) {
  def next(p: Pots) = {
    hp.next(p.hp)
    mp.next(p.mp)
    atk.next(p.atk)
    defs.next(p.defs)
    mag.next(p.mag)
    spr.next(p.spr)
  }
}
object PotSubjects {
  def apply(): PotSubjects = PotSubjects(Subject(), Subject(), Subject(),
    Subject(), Subject(), Subject())
}
case class Pots(hp: Int, mp: Int,
  atk: Int, defs: Int, mag: Int, spr: Int)
object Pots {
  def none = Pots(0,0,0,0,0,0)
}
case class BaseStats(hp: Int, mp: Int, atk: Int, defs: Int, mag: Int, spr: Int, pots: Pots) {
  def asStats = Stats(hp, mp, atk, defs, mag, spr, AilmentResist.zero, ElementResist.zero)
}
case class Stats(hp: Int, mp: Int, atk: Int, defs: Int, mag: Int, spr: Int, status: AilmentResist, element: ElementResist) {
  def +(o: Option[EsperStatInfo]) = Stats(
    hp   + o.fold(0)(_.hp.effectiveMax),
    mp   + o.fold(0)(_.mp.effectiveMax),
    atk  + o.fold(0)(_.atk.effectiveMax),
    defs + o.fold(0)(_.defs.effectiveMax),
    mag  + o.fold(0)(_.mag.effectiveMax),
    spr  + o.fold(0)(_.spr.effectiveMax),
    status, element
  )

  def ++(o: Option[EsperEntry]) = Stats(
    hp, mp, atk, defs, mag, spr,
    status + o.fold(AilmentResist.zero)(_.statusResist),
    element + o.fold(ElementResist.zero)(_.elementResist)
  )

  def +(o: EquipStats) = Stats(
    hp + o.hp,
    mp + o.mp,
    atk + o.atk,
    defs + o.defs,
    mag + o.mag,
    spr + o.spr,
    status + o.ailmentResist,
    element + o.elementResist
  )

  def *(o: PassiveStatEffect) = Stats(
    (hp   * math.min(400, 100.0 + o.hp)   / 100.0).toInt,
    (mp   * math.min(400, 100.0 + o.mp)   / 100.0).toInt,
    (atk  * math.min(400, 100.0 + o.atk)  / 100.0).toInt,
    (defs * math.min(400, 100.0 + o.defs) / 100.0).toInt,
    (mag  * math.min(400, 100.0 + o.mag)  / 100.0).toInt,
    (spr  * math.min(400, 100.0 + o.spr)  / 100.0).toInt,
    status, element
  )

  def *(o: Passive2HEffect) = Stats(
    hp, mp,
    (atk * math.min(300, o.dh / 100.0)).toInt,
    defs,
    mag, spr,
    status, element
  )

  def *(o: PassiveSinglehandEffect) = Stats(
    (hp   * math.min(300, o.hp   / 100.0)).toInt,
    (mp   * math.min(300, o.mp   / 100.0)).toInt,
    (atk  * math.min(300, o.atk  / 100.0)).toInt,
    (defs * math.min(300, o.defs / 100.0)).toInt,
    (mag  * math.min(300, o.mag  / 100.0)).toInt,
    (spr  * math.min(300, o.spr  / 100.0)).toInt,
    status, element
  )

  def +(o: Stats) = Stats(
    hp + o.hp,
    mp + o.mp,
    atk + o.atk,
    defs + o.defs,
    mag + o.mag,
    spr + o.spr,
    status + o.status,
    element + o.element
  )

  def -(o: Stats) = Stats(
    hp - o.hp,
    mp - o.mp,
    atk - o.atk,
    defs - o.defs,
    mag - o.mag,
    spr - o.spr,
    status,
    element
  )
}
object Stats {
  def zero = Stats(0, 0, 0, 0, 0, 0, AilmentResist.zero, ElementResist.zero)
  def fromEquipStats(s: EquipStats) = Stats(s.hp, s.mp, s.atk, s.defs, s.mag, s.spr, s.ailmentResist, s.elementResist)
}

object Data {
  import boopickle.Default._
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

  import java.util.Base64
  def toString[A : Pickler](a: A): String =
    java.nio.charset.StandardCharsets.UTF_8.decode(Base64.getUrlEncoder.encode(Pickle.intoBytes(a))).toString

  def fromString[A : Pickler](s: String): A =
    Unpickle[A].fromBytes(ByteBuffer.wrap(Base64.getUrlDecoder.decode(s)))
}

sealed trait Sort extends Function1[Any,Sort] {
  def apply(any: Any) = this
}

object Sort {
  case object AZ extends Sort
  case object HP extends Sort
  case object MP extends Sort
  case object ATK extends Sort
  case object DEF extends Sort
  case object MAG extends Sort
  case object SPR extends Sort
}

object AbilitySubjects {
  def apply(): AbilitySubjects = AbilitySubjects(Subject[Option[String]](), Subject[Option[String]](), Subject[Option[String]](), Subject[Option[String]]())
}
case class AbilitySubjects(a1: Subject[Option[String]], a2: Subject[Option[String]], a3: Subject[Option[String]], a4: Subject[Option[String]])
}
