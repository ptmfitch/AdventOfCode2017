import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {

  case class Layer(depth: Int, range: Int) {
    override def toString: String = depth + ": " + range
    val severity: Int = depth * range
    def pos(step: Int): Int = {
      val mod: Int = step % (range - 1)
      if((step / (range - 1)) % 2 == 0) mod else (range - 1) - mod
    }
  }

  val layers: Seq[Layer] = Source.fromFile("Day13.txt").getLines.map(s => Layer(s.takeWhile(_.isDigit).toInt, s.dropWhile(_ != ' ').tail.takeWhile(_.isDigit).toInt)).toList

  var danger: Int = 0

  @tailrec
  def stepThroughLayers(step: Int): Unit = {
    val layer = layers.find(l => l.depth == step)
    if(layer.isDefined) {
      if(layer.get.pos(step) == 0) danger += layer.get.severity
      stepThroughLayers(step + 1)
    } else {
      if(step <= layers.maxBy(l => l.depth).depth) stepThroughLayers(step + 1)
    }
  }

  stepThroughLayers(0)

  println(danger)
}
