package permrle

object PermRLE extends App {

  println("Hallo Welt!")

  val s = "abcdabcd".toCharArray.toList
  val k = 4
  val l = Range(0, k).toList.permutations
  while (l.hasNext) {
    val p = l.next
    println(p + ": " + permutate(s, p))
  }

//  val e = encode(s)
//  println(e.size)


  def permutate[A](ls: List[A], p: List[Int]): List[A] =
    ls.grouped(p.size).map(b => permBlock(b,p)).flatten.toList

  private def permBlock[A](ls: List[A], p: List[Int]): List[A] = p.map(i => ls(i))

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
