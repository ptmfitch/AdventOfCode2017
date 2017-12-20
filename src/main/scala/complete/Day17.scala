package complete

import scala.annotation.tailrec

object Day17 extends App {
  val step: Int = 371
  val max: Int = 2017
  @tailrec
  def insert(n: Int = 1, pos: Int = 0, ls: Seq[Int] = Seq(0)): Seq[Int] = {
    val newPos: Int = (pos + step) % n + 1
    val (front, back): (Seq[Int], Seq[Int]) = ls.splitAt(newPos)
    val res: Seq[Int] = front ++ Seq(n) ++ back
    if(n == max) res else insert(n + 1, newPos, res)
  }
  val res: Seq[Int] = insert()
  println(res)
  println(res.apply(res.indexOf(max) + 1))
}

object Day17P2 extends App {
  val step: Int = 371
  val max: Int = 50000000
  var afterZero: Int = 0
  @tailrec
  def numAfterZero(n: Int, pos: Int): Unit = {
    val cur: Int = (pos + step) % n + 1
    if(cur == 1) afterZero = n
    if(n <= max) numAfterZero(n + 1, cur)
  }
  numAfterZero(1, 0)
  println(afterZero)
}
