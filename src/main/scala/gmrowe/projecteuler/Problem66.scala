package gmrowe.projecteuler

import EulerUtils._
import scala.math.{round, sqrt}

object Problem66 {
  // y = sqrt((x^2 - 1) / D)
  def solveFor(d: Long): (Long, Long) = {
    def slv(n: Long): (Long, Long) = {
      if (isPerfectSquare(n + 1) && isPerfectSquare(n / d)) {
        (round(sqrt(n + 1)), round(sqrt(n / d)))
      } else {
        slv(n + d)
      }
    }
    slv(d)
  }
  
  def main(args: Array[String]): Unit = {
    def unnest(nested: (Long, (Long, Long))): (Long, Long, Long) = nested match {
      case (a, (b, c)) => (a, b, c)
    }
    val sols = 
      for { n <- 1L to 1000L
            if !isPerfectSquare(n)
      } yield {
        val res = unnest((n, solveFor(n)))
        println(res)
        res
      }
   
    val s = sols.tail.foldLeft (sols.head) { (max, curr) => 
      (max, curr) match {
      case ((a, b, c), (d, e, f)) =>
        if (b > e) max else curr
      }
    }
    
    println(s)
   
  }
}
