package gmrowe.projecteuler

import scala.collection.immutable.HashSet
import EulerUtils._

object Problem71 {
   
   
   def properRationals(denom: Int, lowerBound: Rational, upperBound: Rational): List[Rational] = {
      val lb = denom * lowerBound.numerator / lowerBound.denominator
      val ub = denom * upperBound.numerator / upperBound.denominator
      for { n <- List.range(lb - 5, ub + 5) } yield {
        new Rational(n, denom)
      }
      
   }
   
   def lastMultBelow(factor: Int, limit: Int): Int = {
       def f(last: Int): Int = {
         val next = last + factor
         if (next > limit) last else factor
       }
       f(factor)
     }

   def divisors: Set[Int] = {
     (1 to 1e6.toInt).filter(isPrime(_)).foldLeft (Set[Int]()) {
        (s, n) => s + lastMultBelow(n, 1e6.toInt)
     }
   }  
   
   def solve: Rational = {
      val max = 1e6.toInt
      val target = new Rational(3, 7)
      val initGuess = new Rational(139999, 980000)
      def f(bestGuess: Rational, denominators: List[Int]): Rational = denominators match {
         case Nil => bestGuess
         case denominator::rest => {
            val rats = properRationals(denominator, bestGuess, target)
            val guess = (rats.takeWhile (_ < target)).last
            val bg = if (guess > bestGuess) guess else bestGuess
            f(bg, rest)
         }
      }
      val divs = divisors.filter ( _ > 980000)
      f(initGuess, divs.toList.sortWith (_ < _))
   }
            
   import util.Timeable.time
   def main(args: Array[String]) {
     println(time(solve))
   }
   
}