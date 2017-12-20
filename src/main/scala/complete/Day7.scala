package complete

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {

  case class BaseNode(name: String, weight: Int, childNames: Seq[String])

  def buildNode(str: String): BaseNode = BaseNode(
    str.takeWhile(_.isLetter),
    str.filter(_.isDigit).toInt,
    if(str.contains(",")) str.dropWhile(_ != '>').split(", ").map(_.filter(_.isLetter)).toList else Seq()
  )

  val nodes: Seq[BaseNode] = Source.fromFile("Day7.txt").getLines().toList.map(buildNode)
  val res1: BaseNode = nodes.find(n => !nodes.exists(_.childNames.contains(n.name))).get
  println(res1.name)

  case class Node(name: String, weight: Int, childNodes: Seq[Node])

  def buildTree(bn: BaseNode): Node = Node(bn.name, bn.weight, bn.childNames.map(s => buildTree(nodes.find(_.name == s).get)))
  def weighChildren(n2: Node): Int = n2.weight + n2.childNodes.map(weighChildren).sum

  @tailrec
  def balanceChildren(n2: Node, diff: Int = 0): Int = {
    val ns: Seq[(String, Int)] = n2.childNodes.map(n => n.name -> weighChildren(n))
    if(ns.map(_._2).distinct.lengthCompare(1) == 0) n2.weight - diff
    else balanceChildren(buildTree(nodes.find(_.name == ns.maxBy(_._2)._1).get), ns.maxBy(_._2)._2 - ns.minBy(_._2)._2)
  }

  val rootNode = buildTree(res1)
  println(balanceChildren(rootNode))
}
