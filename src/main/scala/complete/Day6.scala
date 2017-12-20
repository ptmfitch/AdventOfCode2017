package complete

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Day6 extends App {
  val input: ListBuffer[Int] = ListBuffer[Int]("2\t8\t8\t5\t4\t2\t3\t1\t5\t5\t1\t2\t15\t13\t5\t14".split("\t").map(_.toInt): _*)
  val history: ListBuffer[String] = new ListBuffer[String]

  @tailrec
  def communism(people: ListBuffer[Int], wealth: Int = 0, pos: Int = 0): Int = {
    if(history.contains(people.mkString)) {
      history += people.mkString
      history.length - 1
    }
    else {
      if (wealth == 0) {
        val max = people.max
        val ind = people.indexOf(max)
        history += people.mkString
        people.update(ind, 0)
        communism(people, max, ind + 1)
      } else {
        val p = if(pos < people.length) pos else pos - people.length
        people.update(p, people.apply(p) + 1)
        communism(people, wealth - 1, pos + 1)
      }
    }
  }

  val res1: Int = communism(input)
  println(res1)
  val str = history.apply(res1)
  val res2 = res1 - history.indexOf(str)
  println(res2)

}
