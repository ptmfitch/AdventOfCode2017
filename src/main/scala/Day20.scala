import scala.annotation.tailrec
import scala.io.Source

object Day20 extends App {

  case class Vector3(x: Int, y: Int, z: Int) {
    def manhattanDistance(): Int = math.abs(x) + math.abs(y) + math.abs(z)
    def +(that: Vector3): Vector3 = Vector3(x + that.x, y + that.y, z + that.z)
  }

  case class Particle(var p: Vector3, var v: Vector3, a: Vector3) {
    def accelerate(): Unit = v += a
    def move(): Unit = p += v
  }

  def stringToVector3(str: String): Vector3 = {
    val params = str.filterNot(c => c == '<' || c == '>' || c == '=' || c.isLetter).split(",")
    Vector3(params.apply(0).toInt, params.apply(1).toInt, params.apply(2).toInt)
  }

  val indexedParticles: Seq[(Particle, Int)] = Source.fromFile("input/Day20.txt").getLines().toList.map(s =>  {
    val split = s.split(", ")
    Particle(stringToVector3(split.apply(0)), stringToVector3(split.apply(1)), stringToVector3(split.apply(2)))
  }).zipWithIndex

  val particles: Seq[Particle] = indexedParticles.map(_._1)

  def updatePart1(): Unit = {
    indexedParticles.foreach(p => {
      p._1.accelerate()
      p._1.move()
    })

    println(indexedParticles.minBy(p => p._1.p.manhattanDistance())._2)
  }

  //while(true) {
  //  update()
  //}

  @tailrec
  def updatePart2(ps: Seq[Particle]): Seq[Particle] = {
    ps.foreach(p => {
      p.accelerate()
      p.move()
    })
    val res: Seq[Particle] = ps.filterNot(p => ps.count(c => c.p == p.p) > 1)
    println(res.length)
    updatePart2(res)
  }

  updatePart2(particles)

}
