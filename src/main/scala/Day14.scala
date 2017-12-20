object Day14 extends App {
  val input: String = "jxqlasbh"
  case class HashInput(key: String, row: Int) {
    override def toString: String = key + "-" + row.toString
  }
}
