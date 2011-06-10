package gmrowe.projecteuler

import EulerUtils._

object Problem47 {

   def solve: (Int, Int, Int, Int) = {
      def iterate(curr: Int): (Int, Int, Int, Int) = {
         val step = 4
         val facs = distinctPrimeFactors(curr)
         if (facs.size == 4) {
            checkRange(curr - (step - 1), curr + (step - 1)) match {
               case Some(x) => x
               case None    => iterate(curr + step)
            }
         }
         else iterate(curr + 3)
      }
      
      /*def checkRange(from: Int, to: Int): Option[(Int, Int, Int, Int)] = {
         def checkSizes(values: List[(Int, Int)]): Option[(Int, Int, Int, Int)] = {
            if (values.size < 3) None
            else {
              values match {
                 case w::x::y::z::rest 
                   if (w._2 == 4 && x._2 == 4 && y._2 == 4 && z._2 == 4) =>
                     Some(w._1, x._1, y._1, z._1)
                 case _::rest => checkSizes(rest)
                 case Nil     => None
              }
            }
         }
         checkSizes(List.range(from, to + 1).map (n => (n, distinctPrimeFactors(n).size)))
      }*/
      
      def checkRange(from: Int, to: Int): Option[(Int, Int, Int, Int)] = {
        val target = 4
        def count(values: List[(Int, Int)], acc: List[(Int, Int)]): Option[(Int, Int, Int, Int)] = {
           val size = acc.size
           if (size == target) {
              val (d::c::b::a::_) = acc
              Some((a._1, b._1, c._1, d._1))
           }
           else if (size + values.size < target) None
           else {
             val hd = values.head
             if (hd._2 == target) count(values.tail, hd::acc)
             else count(values.tail, Nil)
           }
        }
        count(List.range(from, to + 1).map (n => (n, distinctPrimeFactors(n).size)), Nil)
      }
      iterate(1)
   }
    
   def main(args: Array[String]): Unit = {
      val res = util.Timeable.time {
        val (a, b, c, d) = solve
        println(a+": "+distinctPrimeFactors(a))
        println(b+": "+distinctPrimeFactors(b))
        println(c+": "+distinctPrimeFactors(c))
        println(d+": "+distinctPrimeFactors(d))
      }
      println(res._2.toDouble / 1000.0 + " seconds")
   }
         
}