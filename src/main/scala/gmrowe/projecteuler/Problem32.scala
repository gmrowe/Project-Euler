/**
  
   The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing
   multiplicand, multiplier, and product is 1 through 9 pandigital.
   
   Find the sum of all products whose multiplicand/multiplier/product identity can
   be written as a 1 through 9 pandigital.
   
   HINT: Some products can be obtained in more than one way so be sure to only
   include it once in your sum.
*/
package gmrowe.projecteuler

import EulerUtils._

object Problem32 {
   
   lazy val pandigitalProducts: List[Int] = {
      val min = 1234
      val max = 9876
      List.range(min, max) filter (hasPandigitalProduct)
   }
   
   def hasPandigitalProduct(n: Int) = {
      def productIsPandigital(ops: (Int, Int)) = {
         val nums = List(ops._1, ops._2, n)
         isPandigital(compose(nums flatMap (decompose(_))))
      }
      factorize(n) exists (productIsPandigital)
   }
   
   lazy val solve = sum(pandigitalProducts)
   
   def main(args: Array[String]) {
     val (res, secs) = util.Timeable.timeSeconds(solve)
     println(res)
     println(secs+" seconds elapsed")
   }
}