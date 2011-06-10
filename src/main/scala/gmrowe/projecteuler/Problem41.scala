package gmrowe.projecteuler

import EulerUtils._

object Problem41 {
   
   def solve: Int = {
      max(primesBelow(7654321).filter (n => isPandigital(n, countDigits(n))))
   }
   
   def main(args: Array[String]) {
      val elapsed = time(println(solve))
      println(elapsed + "ms elapsed")
   }

}