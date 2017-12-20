object Day3 extends App {
  case class Vector2(x: Int, y: Int)
  case class Node(n: Int, coords: Vector2)

  // Y
  // 1   5 4 3
  // 0   6 1 2
  //-1   7 8 9
  //
  //    -1 0 1 X

  val origin: Node = Node(1, Vector2(0, 0))

  def searchGraph(prev: Node = origin, search: Int): Node = {
    val end: Node = Node(
      math.pow(math.sqrt(prev.n) + 2, 2).toInt,
      Vector2(prev.coords.x + 1, prev.coords.y - 1)
    )
    if(end.n > search) {
      val ln: Int = (end.n - prev.n) / 4
      val dist: Int = end.n - search
      val sides: Int = dist / ln
      val searchNode: Node = Node(search, sides match {
        case 0 => Vector2(end.coords.x - (dist - ln * sides),      end.coords.y)
        case 1 => Vector2(end.coords.x - ln,                       end.coords.y + (dist - ln * sides))
        case 2 => Vector2(end.coords.x - ln + (dist - ln * sides), end.coords.y + ln)
        case 3 => Vector2(end.coords.x,                            end.coords.y + ln - (dist - ln * sides))
      })
      searchNode
    } else searchGraph(end, search)
  }

  val res: Node = searchGraph(search = 325489)
  println(math.abs(res.coords.x) + math.abs(res.coords.y))
}
