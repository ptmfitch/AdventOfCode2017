object Day10 extends App {
  var pos: Int = 0
  var skip: Int = 0
  val lengths: Seq[Int] = Seq(14, 58, 0, 116, 179, 16, 1, 104, 2, 254, 167, 86, 255, 55, 122, 244)
  def hash(xs: Seq[Int], n: Int): Seq[Int] = {
    val base: Seq[Int] = {
      if(pos + n < xs.length) {
        xs.slice(pos, pos + n).reverse
      } else {
        (xs.drop(pos) ++ xs.take(n - (xs.length - pos))).reverse
      }
    }
    val rest: Seq[Int] =
      if(pos + n < xs.length) {
        xs.drop(pos + n) ++ xs.take(pos)
      } else {
        xs.slice(n - (xs.length - pos), pos)
      }
    pos += n + skip
    if(pos > xs.length) pos = pos % xs.length
    skip += 1
    base ++ rest
  }

  def run(lns: Seq[Int], xs: Seq[Int] = 0 to 255): Seq[Seq[Int]] = lns match {
    case Nil => Seq()
    case h :: t =>
      val res: Seq[Int] = hash(xs, h)
      res +: run(t, res)
  }

  val res: Seq[Int] = run(lengths).last
  println(res.length)
}
