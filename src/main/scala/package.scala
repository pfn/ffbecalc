import rxscalajs.Observable
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

  def prependNone(idOb: Observable[String]): Observable[Option[String]] =
    Observable.just(None) ++ idOb.map(maybeId)
  def maybeId(id: String): Option[String] =
    if (id.startsWith("--")) None else Some(id)
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

import boopickle.Default._
case class Stats(hp: Int, mp: Int, atk: Int, defs: Int, mag: Int, spr: Int) {
  def +(o: Option[EsperStatInfo]) = Stats(
    hp   + o.fold(0)(_.hp.effectiveMax),
    mp   + o.fold(0)(_.mp.effectiveMax),
    atk  + o.fold(0)(_.atk.effectiveMax),
    defs + o.fold(0)(_.defs.effectiveMax),
    mag  + o.fold(0)(_.mag.effectiveMax),
    spr  + o.fold(0)(_.spr.effectiveMax),
  )

  def +(o: EquipStats) = Stats(
    hp + o.hp,
    mp + o.mp,
    atk + o.atk,
    defs + o.defs,
    mag + o.mag,
    spr + o.spr
  )

  def *(o: PassiveStatEffect) = Stats(
    math.round(hp   * math.min(400, ((100.0 + o.hp))   / 100.0)).toInt,
    math.round(mp   * math.min(400, ((100.0 + o.mp))   / 100.0)).toInt,
    math.round(atk  * math.min(400, ((100.0 + o.atk))  / 100.0)).toInt,
    math.round(defs * math.min(400, ((100.0 + o.defs)) / 100.0)).toInt,
    math.round(mag  * math.min(400, ((100.0 + o.mag))  / 100.0)).toInt,
    math.round(spr  * math.min(400, ((100.0 + o.spr))  / 100.0)).toInt,
  )

  def *(o: PassiveSinglehandEffect) = Stats(
    math.round(hp   * math.min(300, (o.hp   / 100.0))).toInt,
    math.round(mp   * math.min(300, (o.mp   / 100.0))).toInt,
    math.round(atk  * math.min(300, (o.atk  / 100.0))).toInt,
    math.round(defs * math.min(300, (o.defs / 100.0))).toInt,
    math.round(mag  * math.min(300, (o.mag  / 100.0))).toInt,
    math.round(spr  * math.min(300, (o.spr  / 100.0))).toInt,
  )

  def +(o: Stats) = Stats(
    hp + o.hp,
    mp + o.mp,
    atk + o.atk,
    defs + o.defs,
    mag + o.mag,
    spr + o.spr
  )
}
object Stats {
  def zero = Stats(0, 0, 0, 0, 0, 0)
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
}
