package complete

import scala.io.Source

object Day4 extends App {
  val input: Seq[Seq[String]] = Source.fromFile("Day4.txt").getLines().toSeq.map(_.split(" ").toSeq)
  val res1: Int = input.count(ss => ss == ss.distinct)
  println(res1)
  val res2: Int = input.count(ss => ss.map(s => s.sorted).distinct == ss.map(s => s.sorted))
  println(res2)
}
