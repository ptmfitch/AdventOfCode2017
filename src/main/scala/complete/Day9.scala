package complete

import scala.io.Source

object Day9 extends App {
  val input: String = Source.fromFile("Day9.txt").getLines.mkString
  println(input)
  val clean: String = input.replaceAll("!.", "").replaceAll("<.*?>", "")
  println(clean)
  var acc: Int = 0
  var lvl: Int = 0
  def process(cs: Seq[Char]): Unit = cs match {
    case Nil =>
    case h :: t if h == '{' =>
      lvl += 1
      acc += lvl
      process(t)
    case h :: t if h == '}' =>
      lvl -= 1
      process(t)
    case h :: t => process(t)
  }
  process(clean.toList)
  println(acc)
  val noCancel: String = input.replaceAll("!.", "")
  val noGarbage = noCancel.replaceAll("<.*?>", "<>")
  println(math.abs(noCancel.length-noGarbage.length))
}
