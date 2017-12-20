import scala.annotation.tailrec
import scala.io.Source

object Day18 extends App {
  case class Reg(name: Char, var num: Long = 0)

  val X: Int = 1
  val Y: Int = 2

  def getReg(name: Char): Reg = regs.find(_.name == name).get
  def getVal(str: String): Long = if (str.head.isLetter) getReg(str.head).num else str.toInt
  def getIns(n: Long): String = {
    val res: Option[(String, Int)] = ins.find(_._2 == n)
    if (res.isDefined) res.get._1 else "end"
  }

  val ins: Seq[(String, Int)] = Source.fromFile("Day18.txt").getLines.toList.zipWithIndex
  val regs: Seq[Reg] = ('a' to 'z').map(Reg(_))

  @tailrec
  def parseIns(n: Long): Unit = {
    val params = getIns(n).split(" ")
    var skip: Long = 1
    params.apply(0) match {
      case "snd" => lastPlayed = getVal(params.apply(X))
      case "set" => getReg(params.apply(X).head).num =  getVal(params.apply(Y))
      case "add" => getReg(params.apply(X).head).num += getVal(params.apply(Y))
      case "mul" => getReg(params.apply(X).head).num *= getVal(params.apply(Y))
      case "mod" => getReg(params.apply(X).head).num %= getVal(params.apply(Y))
      case "rcv" => if (getVal(params.apply(X)) != 0) return
      case "jgz" => if (getVal(params.apply(X))  > 0) skip = getVal(params.apply(Y))
      case "end" => return
    }
    parseIns(n + skip)
  }


  var lastPlayed: Long = 0
  parseIns(0)
  println(lastPlayed)
}
