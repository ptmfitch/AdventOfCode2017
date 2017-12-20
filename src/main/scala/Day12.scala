import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day12 extends App {
  val input: Seq[String] = Source.fromFile("Day12.txt").getLines().toList
  case class Pipeline(key: Int, links: Seq[Int])
  val network: Seq[Pipeline] = input.map(s => Pipeline(
    s.takeWhile(_.isDigit).toInt,
    s.dropWhile(_ != '>').split(",").toList.map(_.filter(_.isDigit).toInt)
  ))
  println(network)

  var groupZero: ListBuffer[Int] = new ListBuffer[Int]()
  groupZero += 0
  def linksToKey(key: Int): Unit = {
    network.find(_.key == key).get.links.map(k =>
      if(!groupZero.contains(k)) {
        groupZero += k
        linksToKey(k)
      }
    )
  }
  linksToKey(0)
  println(groupZero.length)
  var newNetwork: Seq[Pipeline] = network.filterNot(p => groupZero.contains(p.key))
  println(newNetwork.length)
  println(linksToKey(newNetwork.head.key))

}
