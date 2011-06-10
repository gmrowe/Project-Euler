package gmrowe.projecteuler

import EulerUtils._
import scala.math._

object Problem38 {
   
   def concat(n1: Int, n2: Int): Int = {
      val factor = pow(base, countDigits(n2)).intValue
      (n1 * factor) + n2
   } 
   
   //Map used to trim the search space or at least limit the number of 
   //calculations performed when the solution is impossible for this problem.
   //TODO: Find a better name for this map
   val trim = Map((0, 0), (1, 9), (2, 4), (3, 9), (4, 4),
                  (5, 1), (6, 4), (7, 9), (8, 4), (9, 9))
   
   def pandigitalProduct(n: Int): Option[Int] = {
      val numDigits = 9
      val maxLen = trim(n % base)
      val smallestPandig = 123456789
      val largestPandig = 987654321
      
      def top(curr: Int): Option[Int] = {
         if (curr - 1 > maxLen) {
            None
         } else {
            val nums = List.range(1, curr).map (_ * n)
            val num = nums.reduceLeft (concat)
         
            if (num < smallestPandig && num > 0) {
               top(curr + 1)
            } else if (num > largestPandig || num < 0) {
               None
            } else if (isPandigital(num)) {
               Some(num)
            } else {
               top(curr + 1)
            }
         }
      }
      top(2)
   }
   
   def mostSigDig(n: Int): Int = if (n < base) n else mostSigDig(n / base)
   
   def solve: (Int, Int) = {
      val highestPossibleSolution = 9999
      
      val ansOpts = {
         for (i <- 9 to highestPossibleSolution if mostSigDig(i) == 9) 
         yield (i, pandigitalProduct(i))
      }
      
      val ans = for ((x, Some(a)) <- ansOpts) yield (x, a)//Filter out the Nones
      ans.reduceLeft ((x, y) => if (x._2 > y._2) x else y)
   }
     
   def main(args: Array[String]) { 
      val elapsed = time(println(solve))
      println(elapsed + "ms elapsed")
   }
   
}