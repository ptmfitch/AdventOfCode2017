import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day11 extends App {

  case class Vector2(x: Double, y: Double)

  def stringToVector2(s: String): Vector2 = s match {
    case "n"  => Vector2( 0.0,  1.0)
    case "ne" => Vector2( 0.5,  0.5)
    case "nw" => Vector2(-0.5,  0.5)
    case "s"  => Vector2( 0.0, -1.0)
    case "se" => Vector2( 0.5, -0.5)
    case "sw" => Vector2(-0.5, -0.5)
  }

  val input: Seq[Vector2] = Source.fromFile("Day11.txt").getLines().mkString.split(",").toList.map(stringToVector2)
  var pos: Vector2 = Vector2(0.0, 0.0)
  var max: (Int, Int) = (0, 0)
  var moves :ListBuffer[Vector2] = new ListBuffer
  @tailrec
  def move(xs: Seq[Vector2], cnt: Int = 0): Unit = xs match {
    case Nil =>
    case h :: t =>
      moves += h
      pos = Vector2(pos.x + h.x, pos.y + h.y)
      val p = math.abs(pos.x) + math.abs(pos.y)
      if(p > max._1) max = (p.toInt, cnt)
      move(t, cnt + 1)
  }
  move(input)
  println(pos)
  println(max)

}
