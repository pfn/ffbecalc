package yaffbedb

import boopickle.Default._
import io.circe._
import io.circe.parser._
import java.io.File
import java.io.FilenameFilter
import java.nio.ByteBuffer
import scala.io.Source
import java.nio.channels.FileChannel
import java.nio.file.StandardOpenOption

import DataDecoders._

object Pickler {
  implicit class RichFile(val f: File) extends AnyVal {
    def /(path: String) = new File(f, path)

    def *(filter: String => Boolean) = Option(f.listFiles(new FilenameFilter {
      override def accept(file: File, name: String) = filter(name)
    })).fold(List.empty[File])(_.toList)
  }

  def main(args: Array[String]): Unit = {
    val jsonpath   = new File("json")
    val picklepath = new File("pickle")
    //enhance lb
    val enhancepath = jsonpath / "enhance"
    val equippath   = jsonpath / "equip"
    val esperpath   = jsonpath / "esper"
    val esperbpath  = jsonpath / "esperboard"
    val materiapath = jsonpath / "materia"
    val unitpath    = jsonpath / "unit"
    val skillpath   = jsonpath / "skill"

    (enhancepath * jsonFilter).foreach { f =>
      val n = f.getName
      val out = n.dropRight(5) + ".pickle"
      pickle[Map[String,Enhancement],Map[String,Enhancement]](
        f, picklepath / "enhance" / out,
        _.fold(e => sys.error("unit enhance data: " + e), identity))
    }
    pickle[Map[String,EquipIndexData],List[EquipIndex]](
      equippath / "index.json", picklepath / "equip" / "index.pickle", {
        _.right.map { m =>
          m.toList.map { case (k,v) =>
            EquipIndex(k, v.id, v.twohands.getOrElse(false), v.slotId,
              v.skills,
              v.tpe, v.skilleffects,
              v.skillEffects, v.stats, v.req)
          }.sortBy(_.name)
        }.fold(e => sys.error("equips " + e), identity)
      })
    pickle[Map[String,UnitIndexData],List[UnitIndex]](
      unitpath / "index.json", picklepath / "unit" / "index.pickle", {
        _.right.map { m =>
          m.toList.map { case (k,v) =>
            UnitIndex(k, v.min, v.max, v.id)
          }.sortBy(_.name).filter(u => u.max > 3)
        }.fold(e => sys.error("units " + e), identity)
      })
    pickle[Map[String,MateriaIndexData],List[MateriaIndex]](
      materiapath / "index.json", picklepath / "materia" / "index.pickle", {
        _.right.map { m =>
          m.toList.map { case (k,v) =>
            MateriaIndex(k, util.Try(v.id.toInt).toOption.getOrElse(0),
              v.effects, v.rarity, v.magicType, v.skilleffects)
          }.sortBy(_.name)
        }.fold(e => sys.error("materias: " + e), identity)
      })
    pickle[Map[String,Int],Map[String,Int]](
      esperpath / "index.json", picklepath / "esper" / "index.pickle", 
        _.fold(_ => Map.empty, identity))
    (unitpath * jsonFilter).foreach { f =>
      val n = f.getName
      val out = n.dropRight(5) + ".pickle"
      pickle[UnitData,UnitData](f, picklepath / "unit" / out,
        _.fold(e => sys.error("unit data: " + e), identity))
    }
    (skillpath * jsonFilter).foreach { f =>
      val n = f.getName
      val out = n.dropRight(5) + ".pickle"
      pickle[SkillInfo,SkillInfo](f, picklepath / "skill" / out,
        _.fold(e => sys.error("unit skill data: " + e), identity))
    }
    (esperpath * jsonFilter).foreach { f =>
      val n = f.getName
      val out = n.dropRight(5) + ".pickle"
      pickle[EsperData,EsperData](f, picklepath / "esper" / out,
        _.fold(e => sys.error("esper data: " + e), identity))
    }
    (esperbpath * jsonFilter).foreach { f =>
      val n = f.getName
      val out = n.dropRight(5) + ".pickle"
      pickle[Map[String,EsperSlot],List[EsperSlot]](f,
        picklepath / "esperboard" / out,
        _.fold(e => sys.error("esperboard data: " + e),
          _.toList.sortBy(_._1).collect { case (_,v) if v.reward != UnknownEsperSkill => v }))
    }
    //println(unpickle[List[EquipIndex]](picklepath / "equip" / "index.pickle"))
    println(unpickle[List[EsperSlot]](picklepath / "esperboard" / "4.pickle"))
    //println(unpickle[SkillInfo](picklepath / "skill" / "910274.pickle"))
  }

  val jsonFilter: String => Boolean = {
    case "index.json" => false
    case f if f endsWith ".json" => true
    case _ => false
  }

  def unpickle[A : Pickler](source: File): A = {
    val buf = ByteBuffer.allocate(source.length.toInt)
    val fc = FileChannel.open(source.toPath, StandardOpenOption.READ)
    while (buf.remaining > 0)
      fc.read(buf)
    buf.flip()
    Unpickle[A].fromBytes(buf)
  }
  def pickle[A : Decoder,B : Pickler](source: File, out: File,
    encoder: Either[Error,A] => B): Unit = {
    out.getParentFile.mkdirs()
    val eindex = Source.fromFile(source).getLines.mkString

    val e = Pickle.intoBytes(encoder(decode[A](eindex)))
    val fc = FileChannel.open(out.toPath,
      StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    while (e.remaining > 0)
      fc.write(e)
    fc.close()
  }
}
