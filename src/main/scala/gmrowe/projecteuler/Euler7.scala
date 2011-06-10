package gmrowe.projecteuler

import util.Timeable
import scala.math._

object Euler7 extends Timeable[Int] {
   def primes(howMany: Int): List[Int] = {
     def nextPrime(count: Int, acc: List[Int], lastChecked: Int): List[Int] = {
       if (count >= howMany) acc
       else {
         val curr = lastChecked + 2
         val root = sqrt(curr).toInt
         if (acc.forall (n => n > root || curr % n != 0)) {
            nextPrime(count + 1, curr::acc, curr)
         } else {
            nextPrime(count, acc, curr)
         }
       }
     }
     
     if (howMany == 0) List()
     else if (howMany == 1) List(2)
     else if (howMany == 2) List(3, 2)
     else nextPrime(2, List(3, 2), 3)
   }
   
   
   def main(args: Array[String]): Unit = {
      println(time(primes(10001).head))
   }
}