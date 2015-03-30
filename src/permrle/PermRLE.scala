package permrle

object PermRLE extends App {

  println("Hallo Welt!")

  val s = "aabcaaaa"
  val e = encode(s)
  println(e.size)

  def encode(s: String): List[(Char, Int)] =
    s.groupBy(c => c).map(x => (x._1, x._2.le))
}
