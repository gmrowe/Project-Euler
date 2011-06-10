package gmrowe.projecteuler
import EulerUtils.isPrime

import util.Timeable.time

object Euler58 {
   def diagonalNumbers(threshold: Double): Int = {
      def nextN(sideLength: Int, cornerNumber: Int, last: Int, primeCount: Int,
           totalCount: Int): Int =  {
          val next = sideLength - 1 + last
          val nextPrimeCount = if (isPrime(next)) primeCount + 1 else primeCount
          val lastCorner = cornerNumber == 4
          val nextCorner = if (lastCorner) 1 else cornerNumber + 1
          val nextSideLen = if (lastCorner) sideLength + 2 else sideLength
          if (nextPrimeCount.toDouble / (totalCount + 1) < threshold && totalCount > 1) {
             sideLength
          } else {
             nextN(nextSideLen, nextCorner, next, nextPrimeCount, totalCount + 1)
          }
      }
      nextN(3, 1, 1, 0, 1)
   }
   
   def main(args: Array[String]){
      println(time(diagonalNumbers(0.1)))
   }
      
}