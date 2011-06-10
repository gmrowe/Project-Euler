package gmrowe.projecteuler

import EulerUtils._

object Problem193 {
  val limit = longPow(2L, 50L)
  
  def longPow(base: Long, exp: Long): Long = {
    def lp(e: Long, acc: Long): Long = {
      if (e == 0) acc else lp(e - 1, acc * base)
    }
    if (exp < 0) error("Undefined for negative powers") else lp(exp, 1)
  }
  
  lazy val squareOfPrimes : Stream[Int] =  {
    def sieve(nums : Stream[Int], table : Map[Int, List[Int]]): Stream[Int] = {        
       val curr = nums.head
       table.get(curr) match { 
          case None    => 
            Stream.cons(curr * curr, sieve(nums.tail, table + (curr * curr -> List(curr)))) 
          case Some(v) => 
            sieve(nums.tail, v.foldLeft (table - curr) (reinsert (curr) _ ))
       }
    }
    
    def reinsert(curr : Int) (table : Map[Int, List[Int]], prime : Int): Map[Int, List[Int]] = {
       val newVal = prime + curr
       table.get(newVal) match {
          case None => table + (newVal -> List(prime))
          case Some(v) => table + (newVal -> (prime::v))
       }
    }
    sieve(Stream.from(2), Map.empty)
  }
  
  def isEven(n: Long): Boolean = (n & 1) == 0
  
  //Since there is only one even number that is the square of a prime, we can
  //special case even numbers
  def isSquarefree(n: Long): Boolean = {
    (isEven(n) && n % 4L != 0) ||
      (squareOfPrimes takeWhile (_ < nearSqrt(n)) forall (x => n % x.toLong != 0)) 
  }
     
   
  
  def main(args: Array[String]): Unit = {
    import util.Timeable.timeSeconds
    val (res, secs) = timeSeconds {
      def check(n: Long, count: Int): Int =
        if (n > limit) count else check(n + 1, if (isSquarefree(n)) count + 1 else count)
      check(1L, 0)
    }
    println(res)
    println(secs + " seconds elapsed")
  }
}
