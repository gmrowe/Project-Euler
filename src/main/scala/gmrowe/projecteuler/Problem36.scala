package gmrowe.projecteuler

import EulerUtils._

object Problem36 {
   
   def toBinary(n: Int): List[Int] = {
      val bs = java.lang.Integer.toBinaryString(n)
      bs.foldRight (List[Int]()) ((x, l) => Integer.parseInt(String.valueOf(x))::l)
   }
   
   lazy val solve: List[Int] = {
      def qualifies(n: Int) = isPalindrome(decompose(n)) && isPalindrome(toBinary(n))
      List.range(0, 1e6.toInt).filter (qualifies)
   }
   
   def main(args: Array[String]) { println(solve.reduceLeft (_ + _)) }
}