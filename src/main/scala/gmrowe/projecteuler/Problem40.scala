package gmrowe.projecteuler

import EulerUtils._

object Problem40 {

   def rpts: Stream[Int] = {
      def r(n: Int): Stream[Int] = {
         decompose(n).toStream.append (r(n + 1))
      }
      r(1)
   }
   
   def rptIdx(idx: Int): Int = {
      rpts.drop(idx - 1).head
   }
   
   def solve: Int = {
      rptIdx(1) * rptIdx(10) * rptIdx(100) * rptIdx(1000) * rptIdx(10000) * rptIdx(100000) * rptIdx(1000000)
   }
   
   def main(args: Array[String]) {
      val elapsed = time(println(solve))
      println(elapsed + "ms elapsed")
   }

}