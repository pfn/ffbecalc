import rxscalajs.Observable
import java.util.UUID

package object yaffbedb {
  // why isn't the definition in outwatch.dom visible to us?
  type Handler[T] = Observable[T] with outwatch.Sink[T]
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
