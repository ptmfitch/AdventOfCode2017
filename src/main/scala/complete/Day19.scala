package complete

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day19 extends App {

  case class Vector2(x: Int, y: Int) {
    def add(a: Vector2): Vector2 = Vector2(this.x + a.x, this.y + a.y)
  }
  val N: Vector2 = Vector2(0, -1)
  val S: Vector2 = Vector2(0, +1)
  val E: Vector2 = Vector2(+1, 0)
  val W: Vector2 = Vector2(-1, 0)


  case class Maze(matrix: Seq[Array[Char]]) {
    def charAt(pos: Vector2): Char = {
      if (0 <= pos.y && pos.y < matrix.length) {
        val line: Array[Char] = matrix.apply(pos.y)
        if (0 <= pos.x && pos.x < line.length) line.apply(pos.x) else ' '
      } else ' '
    }
  }

  val maze: Maze = Maze(Source.fromFile("Day19.txt").getLines.toList.map(_.toCharArray))

  var pos: Vector2 = Vector2(1, 0)
  var dir: Vector2 = S
  var seen: ListBuffer[Char] = new ListBuffer[Char]

  var cnt: Int = 0

  while(maze.charAt(pos) != ' ') {
    cnt += 1
    maze.charAt(pos) match {
      case '+' if dir == N || dir == S =>
        val charEast: Char = maze.charAt(pos.add(E))
        if (charEast == '-' || charEast.isLetter) dir = E else dir = W
      case '+' if dir == E || dir == W =>
        val charNorth: Char = maze.charAt(pos.add(N))
        if (charNorth == '|' || charNorth.isLetter) dir = N else dir = S
      case c if c.isLetter => print(c)
      case _ =>
    }
    pos = pos.add(dir)
  }
  println
  println(cnt)

}
