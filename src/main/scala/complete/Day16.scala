import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day16 extends App {
  var cur: ListBuffer[Char] = new ListBuffer()
  cur = ListBuffer.empty ++= ('a' to 'p')
  def dance(cs: ListBuffer[Char]): Unit = {
    val ss: Seq[String] = Source.fromFile("Day16.txt").getLines().mkString.split(",").toList
    ss.foreach(s => s.head match {
      case 's' => spin(s.tail.toInt)
      case 'x' => exchange(s.tail.split("/").head.toInt, s.tail.split("/").tail.head.toInt)
      case 'p' => partner(s.tail.split("/").head.head, s.tail.split("/").tail.head.head)
    })
  }
  def spin(n: Int): Unit = {
    val end: ListBuffer[Char] = cur.takeRight(n)
    val start: ListBuffer[Char] = cur.take(cur.length - n)
    cur = end ++ start
  }
  def exchange(a: Int, b: Int): Unit = {
    val aC = cur.apply(a)
    val bC = cur.apply(b)
    cur.update(a, bC)
    cur.update(b, aC)
  }
  def partner(a: Char, b: Char): Unit = {
    val aN = cur.indexOf(a)
    val bN = cur.indexOf(b)
    cur.update(aN, b)
    cur.update(bN, a)
  }
  0 to 999999999 foreach(n => {
    println(n)
    dance(cur)
  })
  println(cur.mkString)
}

object Day16P2 extends App {
  val loops: Int = 999999999
  val ss: Seq[String] = Source.fromFile("Day16.txt").getLines().mkString.split(",").toList
  val instructions: Seq[Seq[Int]] = ss.map(s => s.head match {
    case 's' => Seq(s.head.toInt, s.tail.toInt)
    case 'x' => Seq(s.head.toInt, s.tail.split("/").head.toInt, s.tail.split("/").tail.head.toInt)
    case 'p' => Seq(s.head.toInt, s.tail.split("/").head.head.toInt, s.tail.split("/").tail.head.head.toInt)
  })
  @tailrec
  def dance(ins: Seq[Seq[Int]], cs: String): String = {
    if(ins.nonEmpty) ins.head match {
      case h if h.head == 's' => dance(ins.tail, spin(cs, h.tail.head))
      case h if h.head == 'x' => dance(ins.tail, exchange(cs, h.tail.head, h.tail.tail.head))
      case h if h.head == 'p' => dance(ins.tail, partner(cs, h.tail.head, h.tail.tail.head))
    } else cs
  }
  def spin(cs: String, n: Int): String = cs.takeRight(n) ++ cs.take(16 - n)
  def exchange(cs: String, a: Int, b: Int): String = {
    val aC = cs.charAt(a)
    cs.updated(a, cs.charAt(b)).updated(b, aC)
  }
  def partner(cs: String, a: Int, b: Int): String = {
    val bN = cs.indexOf(b.toChar)
    cs.updated(cs.indexOf(a.toChar), b.toChar).updated(bN, a.toChar)
  }
  @tailrec
  var seen: ListBuffer[String] = new ListBuffer[String]()
  def run(str: String, cnt: Int): Unit = cnt match {
    case n if n == loops => println(str)
    case n if n < loops => {
      val next: String = dance(instructions, str)
      if(seen.contains(next)) {
        println(seen.apply(loops % n))
        return
      } else seen.append(next)
      run(next, cnt - 1)
    }
  }
  run(('a' to 'p').mkString, 0)
}