package gmrowe.projecteuler

import EulerUtils._

/**
* The arithmetic sequence, 1487, 4817, 8147, in which each of the terms 
* increases by 3330, is unusual in two ways: (i) each of the three terms are
* prime, and, (ii) each of the 4-digit numbers are permutations of one another.
*
* There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
* exhibiting this property, but there is one other 4-digit increasing sequence.
*
* What 12-digit number do you form by concatenating the three terms in this
* sequence?
**/
object Problem49 {

  def test(a: Int, b: Int, c: Int): Boolean = {
     List(a, b, c).sortWith(_ < _) match {
        case x::y::z::Nil if (y - x == z - y) => true
        case _ => false
     }
  }
  
  def solve: List[(Int, Int, Int)] = {
    def f(n: Int, seen: Set[Int]): List[(Int, Int, Int)] = {
      if (n > 9999) {
        Nil
      } else if (seen(n)) {
        f(n + 1, seen)
      } else if (!isPrime(n)) {
        f(n + 1, seen + n)
      } else {
        val ps = permutations(decompose(n)).map (compose(_))
        val perms = Set(ps: _*)
        val primePerms = perms filter (isPrime(_))
        if (primePerms.size < 3) {
          f(n + 1, seen ++ perms) 
        } else {
          val arr = primePerms.toArray
          val res = 
            for { k <- 2 to arr.size - 1
                  j <- 1 to k - 1
                  i <- 0 to j - 1 
                  if test(arr(i), arr(j), arr(k)) } yield {(arr(i), arr(j), arr(k))}
          res.toList ++ f(n + 1, seen ++ perms)
        }
      }
    }
    f(1000, Set())
  }
  
  def main(args: Array[String]): Unit = {
     for ((a, b, c)  <- solve) {
        println(a+", "+b+", "+c+"; difference = "+(a.max(b) - a.min(b)))
     }
  }
}