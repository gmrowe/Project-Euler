package gmrowe.projecteuler

import scala.collection.mutable.ListBuffer

object Problem101 {
  def swapPos[A](xs: List[A], fromIdx: Int, toIdx: Int): List[A] = {
    val buf = xs ++=: new ListBuffer
    val temp = buf(fromIdx)
    buf.update(fromIdx, buf(toIdx))
    buf.update(toIdx, temp)
    buf.toList
  }
}