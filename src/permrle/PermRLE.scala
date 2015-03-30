package permrle

object PermRLE extends App {

  println("Hallo Welt!")

  val s = "aabcaaaa"
  val e = encode(s.toCharArray.toList)
  println(e.size)

  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[A](ls: List[A]): List[(A, Int)] =
    pack(ls) map { e => (e.head, e.length) }
}
