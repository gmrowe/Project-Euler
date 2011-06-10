package gmrowe.projecteuler

import EulerUtils._

/**
  A number chain is created by continuously adding the square of the digits in a
  number to form a new number until it has been seen before.
  
  For example,
  
  44 -> 32 -> 13 -> 10 -> 1 -> 1
  85 -> 89 -> 145 -> 42 -> 20 -> 4 -> 16 -> 37 -> 58 -> 89

  Therefore any chain that arrives at 1 or 89 will become stuck in an endless
  loop. What is most amazing is that EVERY starting number will eventually arrive
  at 1 or 89.

  How many starting numbers below ten million will arrive at 89?

*/
object Problem92 {
   val Unknown = 0
   val Has1 = 1
   val Has89 = 89
   
   
   def solve: Int = {
      def s(bs: Array[Int], seen: List[Int], curr: Int): Array[Int] = {
         if (bs(curr) == Unknown) {
            val next = decompose(curr).foldLeft (0) ((acc, n) => acc + (n * n))
            s(bs, curr::seen, next)
         } else {
            val state = bs(curr)
            for (n <- seen) {
               bs(n) = state
            }
            bs
         }
      }
      val seed = Array.fill(1e7.toInt + 1)(Unknown)
      seed(1) = Has1
      seed(89) = Has89
      
      val res = 
         (1e7.toInt to 1 by -1).foldLeft (seed) ((arr, n) => s(arr, List[Int](), n))
         
      res.foldLeft (0) ((acc, n) => if (n == Has89) acc + 1 else acc)
   }
   
   import util.Timeable.time
   def main(args: Array[String]): Unit = {
      println(time(solve))
   }

}