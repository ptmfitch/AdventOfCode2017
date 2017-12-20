package complete

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {

  val input: Seq[Seq[Int]] = Source.fromFile("Day2.txt").getLines.map(_.split("\t").map(_.toInt).toSeq).toSeq
  val res1: Int = input.map(s => s.max - s.min).sum
  println(res1)

  @tailrec
  def divideFactors(ints : Seq[Int]): Int = {
    val res = ints.tail.find(i => ints.head % i == 0)
    if(res.isDefined) ints.head / res.get else divideFactors(ints.tail)
  }
  println(input.map(ss => divideFactors(ss.sorted.reverse)).sum)

}
