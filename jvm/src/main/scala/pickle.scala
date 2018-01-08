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
        _.fold(e => sys.error(s"unit enhance data $n: " + e), identity))
    }
    pickle[Map[String,EquipIndexData],List[EquipIndex]](
      equippath / "index.json", picklepath / "equip" / "index.pickle", {
        _.right.map { m =>
          m.toList.map { case (k,v) =>
            EquipIndex(k, v.id, v.icon, v.twohands,
              v.slotId, v.variance, v.accuracy, v.skills,
              v.tpe, v.stats, v.req, v.skillInfo)
          }.sortBy(_.name)
        }.fold(e => sys.error("equips " + e), identity)
      })
    pickle[Map[String,UnitIndexData],List[UnitIndex]](
      unitpath / "index.json", picklepath / "unit" / "index.pickle", {
        _.right.map { m =>
          m.toList.map { case (k,v) =>
            UnitIndex(k, v.min, v.max, v.id)
          }.filter(u => u.max > 3).sortBy(_.name)
        }.fold(e => sys.error("units " + e), identity)
      })
    pickle[Map[String,MateriaIndexData],List[MateriaIndex]](
      materiapath / "index.json", picklepath / "materia" / "index.pickle", {
        _.right.map { m =>
          m.toList.map { case (k,v) =>
            MateriaIndex(k, util.Try(v.id.toInt).toOption.getOrElse(0),
              v.icon, v.unique, v.rarity, v.magicType, v.skillInfo)
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
        _.fold(e => sys.error(s"unit data $n: " + e), identity))
    }
    (skillpath * jsonFilter).foreach { f =>
      val n = f.getName
      val out = n.dropRight(5) + ".pickle"
      pickle[SkillInfo,SkillInfo](f, picklepath / "skill" / out,
        _.fold(e => sys.error(n + " unit skill data: " + e), identity))
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
    //println(unpickle[List[EsperSlot]](picklepath / "esperboard" / "4.pickle"))
    val equips = unpickle[List[EquipIndex]](picklepath / "equip" / "index.pickle")
    asserteq(equips.find(_.id == 315020900), "Some(EquipIndex(Fixed Dice,315020900,item_11314.png,true,1,WeaponVariance(1.2,6.5),10,List(),13,ATK+1,None,List()))")
    asserteq(equips.find(_.id == 303002300).map(_.skillInfo.flatMap(_.passives).map(_.restrictions)), "Some(List(Set(100000102)))")
    asserteq(equips.find(_.id == 301001500), "Some(EquipIndex(Swordbreaker,301001500,item_10110.png,false,1,WeaponVariance(0.95,1.05),0,List(206170),1,ATK+43,None,List(IndexSkillInfo(206170,Swordbreaker,true,ability_97.png,List(5% chance of evading physical attacks),List(),List(PassiveDodgeEffect(5,0))))))")
    asserteq(equips.find(_.id == 302003300), "Some(EquipIndex(Onion Sword,302003300,item_10230.png,false,1,WeaponVariance(0.9,1.1),0,List(200440, 211500),2,ATK+135,None,List(IndexSkillInfo(200440,Bladeblitz,false,ability_53.png,List(Physical damage (1.4x, ATK) to all enemies),List(Physical*  damage (1.40x ATK) to all enemies),List()), IndexSkillInfo(211500,Onion Cutter,false,ability_54.png,List(Physical damage (5.2x, ATK) to one enemy),List(Physical*  damage (5.20x ATK) to an enemy),List()))))")
    //println(unpickle[SkillInfo](picklepath / "skill" / "910274.pickle"))
    asserteq(unpickle[SkillInfo](picklepath / "skill" / "910523.pickle"), "SkillInfo(910523,Enigmatic,false,false,ABILITY,ability_76.png,None,0,List(),List(PassiveTDHEffect(0,0,0,0,100,100)),List(Increase equipment MAG and SPR (100%) when armed with a single weapon))")
    asserteq(unpickle[SkillInfo](picklepath / "skill" / "20200.pickle"), "SkillInfo(20200,Firaga,false,true,MAGIC,ability_21.png,Some(Black),20,List(Magical Fire damage (1.80x MAG) to all enemies),List(),List(Magic fire damage (1.8x, MAG) to all enemies))")
  }

  def asserteq[A](a: A, b: A) = if (a.toString != b.toString) sys.error(s"\n[$a] != \n[$b]")

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
