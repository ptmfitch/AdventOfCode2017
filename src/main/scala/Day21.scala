import scala.io.Source

object Day21 extends App {

  val tC: Char = '#'
  val fC: Char = '.'

  type Row = Seq[Boolean]
  type Grid = Seq[Row]

  case class Image(grid: Grid) {
    override def toString: String = grid.map(_.map(b => if(b) tC else fC).mkString).mkString("\\")
    def size: Int = grid.length
    def breakDown: Seq[Seq[Image]] = {
      if(size % 2 == 0) {
        grid.sliding(2, 2).toList.map(ls => ls.head.zip(ls.tail.head).sliding(2, 2).toList.map(ls => Image(Seq(Seq(ls.head._1, ls.tail.head._1), Seq(ls.head._2, ls.tail.head._2)))))
      } else {
        grid.sliding(3, 3).toList.map(rs => rs.map(_.sliding(3, 3).toList)).map(ls => ls.head.zip(ls.tail.head).zip(ls.tail.tail.head).map(t => Image(Seq(t._1._1, t._1._2, t._2))))
      }
    }
  }

  val input: Image = Image(Seq(Seq(false, true, false), Seq(false, false, true), Seq(true, true, true)))

  val rules: Seq[(String, String)] = Source.fromFile("input/Day21.txt").getLines.toList.map(s => s.split(" => ").toList).map(ss => (ss.head, ss.tail.head))

}
