package complete

import scala.io.Source

object Day8 extends App {

  case class Register(reg: String, var n: Int = 0)

  case class Instruction(reg: Register, op: Int, con: (Register, String, Int))

  var max: Int = 0

  val registers: Seq[Register] = Source.fromFile("Day8.txt").getLines().map(s => Register(s.takeWhile(_.isLetter))).toList.distinct

  val instructions: Seq[Instruction] = Source.fromFile("Day8.txt").getLines().map(s => {
    val split = s.split(" ")
    Instruction(
      registers.find(_.reg == split.apply(0)).get,
      split.apply(2).toInt * (if(split.apply(1) == "inc") 1 else -1),
      (registers.find(_.reg == split.apply(4)).get, split.apply(5), split.apply(6).toInt)
    )
  }).toList

  def evalCon(reg: Register, op: String, num: Int): Boolean = {
    val n = reg.n
    op match {
      case ">"  => n >  num
      case "<"  => n <  num
      case ">=" => n >= num
      case "<=" => n <= num
      case "==" => n == num
      case "!=" => n != num
    }
  }

  def execute(ins: Instruction): Unit =
    if(evalCon(ins.con._1, ins.con._2, ins.con._3)) {
      ins.reg.n += ins.op
      if(ins.reg.n > max) max = ins.reg.n
    }

  instructions.map(execute)
  println("Answer 1: " + registers.maxBy(_.n).n)
  println("Answer 2: " + max)
}
