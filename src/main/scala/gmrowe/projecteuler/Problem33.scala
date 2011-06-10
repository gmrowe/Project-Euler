package gmrowe.projecteuler

import EulerUtils._

object Problem33 {
   
   lazy val unorthodoxFractions: List[Rational] = {
      val rng = List.range(1, 10)
      var res = List[Rational]()
      for (i <- rng;
           j <- rng if i != j;
           k <- rng if k != i && k != j) {
        val r1 = new Rational (i, j)
        val r2 = new Rational (base * i + k, base * k + j)
        if (r1 == r2) {
           res = r2::res
        }
      }
      res
   }
   
   def main(args: Array[String]) { 
      val product = unorthodoxFractions.reduceLeft (_ * _)
      println(product.denominator)
   }
}