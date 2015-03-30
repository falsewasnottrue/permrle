package permrle

object PermRLE extends App {

  import scala.io.Source
  val lines = Source.fromFile("src/permrle/D-small-practice.in").getLines
  lines.next // drop #cases
  var c = 1
  while (lines.hasNext) {
    val k = lines.next.toInt
    val s = lines.next.toCharArray.toList

    println("Case #" + c + ": " + minPermRLE(k, s))
  }

  def minPermRLE[A](k: Int, ls: List[A]): Int = {
    val l = Range(0, k).toList.permutations
    l.map(p => encode(permutate(ls, p)).size).min
  }

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
