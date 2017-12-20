import scala.collection.mutable.ListBuffer

object Day15 extends App {
  def nextVal(prev: Int, factor: Int): Int = (prev * factor) % 2147483647
  var aReg: Int = 679
  var bReg: Int = 771
  val aFac: Int = 16807
  val bFac: Int = 48271
  var aList: ListBuffer[Long] = new ListBuffer[Long]
  var bList: ListBuffer[Long] = new ListBuffer[Long]
  var cnt = 0
  while(aList.lengthCompare(5000000) < 0 && bList.lengthCompare(5000000) < 0) {
    aReg = nextVal(aReg, aFac)
    bReg = nextVal(bReg, bFac)
    val a = aReg.toBinaryString
    val b = bReg.toBinaryString
    if (a.takeRight(2) == "00") aList += a.takeRight(16).toLong
    if (b.takeRight(3) == "000") bList += b.takeRight(16).toLong
  }
  println("Lists generated...")
  println("Comparing...")
  aList.zip(bList).foreach(t => {
    if(t._1 == t._2) cnt += 1
  })
  println(cnt)
}
