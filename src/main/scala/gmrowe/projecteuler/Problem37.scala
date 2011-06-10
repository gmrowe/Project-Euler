package gmrowe.projecteuler

import EulerUtils._

object Problem37 {

   def truncs(n: Int): List[Int] = {
      val nAsList = decompose(n)
      val len = nAsList.length
      
      def splitCompose(x: Int): List[Int] = {
         val (f, b) = nAsList splitAt x
         List(compose(f), compose(b))
      }
      
      val t = List.range(1, len).flatMap (splitCompose) 
      n::t
   }
   
   def solve: Stream[Int] = Stream.from(10).filter (x => truncs(x).forall (isPrime))
   
   def main(args: Array[String]) { 
      val elapsed = time(println(sum(solve.take(11).force)))
      println(elapsed + " ms elapsed")
   }
   
}