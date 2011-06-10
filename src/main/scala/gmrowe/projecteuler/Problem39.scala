package gmrowe.projecteuler

import EulerUtils._

object Problem39 {

     def sum3(n: Int): List[(Int, Int, Int)] = {
        for { 
           hyp <- List.range((n / 3) + 1, n - 1)
           a <- List.range(1, ((n - hyp) / 2) + 1) if (n - hyp - a < hyp)
         } yield(a, n - hyp - a, hyp)
     }
     
     def isPythagTriple(trip: (Int, Int, Int)) = {
        trip match {
           case (a, b, c) => (a * a) + (b * b) == (c * c)
        }
     }
     
     def solve: (Int, List[(Int, Int, Int)]) = {
        def filterPythagTrips(n: Int) = sum3(n) filter (isPythagTriple)
        val ans = for (p <- 3 to 1000) yield (p, filterPythagTrips(p))
        ans.reduceLeft ((x, y) => if (x._2.length > y._2.length) x else y)
     }
        
     
     def main(args: Array[String]) = {
        val elapsed = time(println(solve))
        println(elapsed + "ms elapsed")
     }
}