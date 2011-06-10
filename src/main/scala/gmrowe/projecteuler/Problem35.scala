package gmrowe.projecteuler

import EulerUtils._

object Problem35 {
   
   def allRotations(n: Int): Set[Int] = {
      def rots(next: => Int, remaining: Int): Set[Int] = {
         if (remaining == 0) Set.empty
         else Set(next) ++ rots(rotateLeft(next), remaining - 1)
      }
      val digitsInN = log10(n).toInt + 1
      rots(n, digitsInN)
   }
   
   lazy val solve: Set[Int] = {
      val lim = 1e6.toInt
      val inRangeSet = Set.empty[Int] ++ primesBelow(lim)
      inRangeSet.foldLeft (Set.empty[Int]) { (s, n) => 
         if (decompose(n).contains (0)) {
            s
         } else {
            val rots = allRotations(n)
            if (rots.forall (n => inRangeSet.contains(n) || isPrime(n))) {
               s ++ (rots.filter (_ < lim))
            } else {
               s
            }
         }
      }
    }
   
   def main(args: Array[String]) {
      println(solve.size)
   }
}
